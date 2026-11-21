package org.hswebframework.ezorm.rdb.xa;

import javax.sql.XAConnection;
import javax.sql.XADataSource;
import javax.transaction.xa.XAException;
import javax.transaction.xa.XAResource;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.sql.SQLException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;

/** Observes the real driver's XA interface; all successful operations reach Kingbase. */
final class ObservedXADataSource implements XADataSource {
    private final XADataSource delegate;
    final AtomicInteger prepares = new AtomicInteger();
    final AtomicInteger twoPhaseCommits = new AtomicInteger();
    final AtomicInteger rollbacks = new AtomicInteger();
    final AtomicInteger recoveries = new AtomicInteger();
    final AtomicBoolean rejectNextPrepare = new AtomicBoolean();
    volatile Runnable afterPrepare = () -> {};
    volatile Runnable afterCommit = () -> {};

    ObservedXADataSource(XADataSource delegate) {
        this.delegate = delegate;
    }

    @Override
    public XAConnection getXAConnection() throws SQLException {
        return observe(delegate.getXAConnection());
    }

    @Override
    public XAConnection getXAConnection(String user, String password) throws SQLException {
        return observe(delegate.getXAConnection(user, password));
    }

    private XAConnection observe(XAConnection connection) throws SQLException {
        XAResource resource = connection.getXAResource();
        XAResource observed = (XAResource) Proxy.newProxyInstance(XAResource.class.getClassLoader(),
            new Class<?>[]{XAResource.class}, (proxy, method, args) -> {
                if ("prepare".equals(method.getName()) && rejectNextPrepare.compareAndSet(true, false)) {
                    // Simulate a resource-manager prepare failure; the coordinator must roll back.
                    throw new XAException(XAException.XAER_RMERR);
                }
                Object result = invoke(resource, method, args);
                switch (method.getName()) {
                    case "prepare" -> {
                        prepares.incrementAndGet();
                        afterPrepare.run();
                    }
                    case "commit" -> {
                        if (Boolean.FALSE.equals(args[1])) twoPhaseCommits.incrementAndGet();
                        afterCommit.run();
                    }
                    case "rollback" -> rollbacks.incrementAndGet();
                    case "recover" -> recoveries.incrementAndGet();
                    default -> { }
                }
                return result;
            });
        return (XAConnection) Proxy.newProxyInstance(XAConnection.class.getClassLoader(),
            new Class<?>[]{XAConnection.class}, (proxy, method, args) ->
                "getXAResource".equals(method.getName()) ? observed : invoke(connection, method, args));
    }

    private static Object invoke(Object target, Method method, Object[] args) throws Throwable {
        try {
            return method.invoke(target, args);
        } catch (InvocationTargetException error) {
            throw error.getCause();
        }
    }

    @Override public PrintWriter getLogWriter() throws SQLException { return delegate.getLogWriter(); }
    @Override public void setLogWriter(PrintWriter writer) throws SQLException { delegate.setLogWriter(writer); }
    @Override public void setLoginTimeout(int seconds) throws SQLException { delegate.setLoginTimeout(seconds); }
    @Override public int getLoginTimeout() throws SQLException { return delegate.getLoginTimeout(); }
    @Override public Logger getParentLogger() throws java.sql.SQLFeatureNotSupportedException { return delegate.getParentLogger(); }
}
