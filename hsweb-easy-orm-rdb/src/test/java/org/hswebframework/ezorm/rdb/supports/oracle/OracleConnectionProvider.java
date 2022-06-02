package org.hswebframework.ezorm.rdb.supports.oracle;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.ConnectionProvider;
import org.hswebframework.ezorm.rdb.Containers;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.wait.strategy.Wait;

import java.sql.Connection;
import java.sql.DriverManager;
import java.util.Queue;
import java.util.concurrent.Callable;
import java.util.concurrent.LinkedBlockingQueue;

public class OracleConnectionProvider implements ConnectionProvider {

    static int port;

    static Queue<Connection> connectionQueue = new LinkedBlockingQueue<>();

    static Callable<Connection> connectionSupplier;

    static {
        try {
            Class.forName("oracle.jdbc.driver.OracleDriver");
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
        GenericContainer<?> container = Containers.newOracle();

        container.start();
        port = container.getMappedPort(1521);

        String username = System.getProperty("oracle.username", "system");
        String password = System.getProperty("oracle.password", "oracle");
        String url = System.getProperty("oracle.url", "127.0.0.1:" + port);
        String db = System.getProperty("oracle.db", "orcl");
        connectionSupplier = () -> DriverManager.getConnection("jdbc:oracle:thin:@" + url + ":" + db, username, password);
        for (int i = 0; i < 10; i++) {
            try {
                connectionQueue.add(connectionSupplier.call());
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
    }

    @SneakyThrows
    public Connection getConnection() {
        Connection connection = connectionQueue.poll();
        return connection == null ? connectionSupplier.call() : connection;
    }

    @Override
    @SneakyThrows
    public void releaseConnect(Connection connection) {
        connectionQueue.add(connection);
    }
}
