package org.hswebframework.ezorm.rdb.xa;

import com.atomikos.icatch.config.Configuration;
import com.atomikos.icatch.jta.UserTransactionImp;
import com.atomikos.icatch.jta.UserTransactionManager;
import com.atomikos.jdbc.AtomikosDataSourceBean;
import com.kingbase8.xa.KBXADataSource;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.springframework.transaction.jta.JtaTransactionManager;
import org.springframework.transaction.support.TransactionTemplate;

import java.sql.Connection;
import java.util.concurrent.atomic.AtomicInteger;

/** Separate JVM for a real abrupt exit, followed by a separate recovery JVM. */
public final class XaRecoveryProcess {
    private XaRecoveryProcess() { }

    public static void main(String[] args) {
        try {
            run(args);
            System.exit(0);
        } catch (Throwable failure) {
            failure.printStackTrace();
            System.exit(1);
        }
    }

    private static void run(String[] args) throws Exception {
        String stage = args[0], logs = args[1], coordinatorName = args[2], table = "\"" + args[3] + "\"", id = args[4];
        System.setProperty("com.atomikos.icatch.log_base_dir", logs);
        System.setProperty("com.atomikos.icatch.tm_unique_name", coordinatorName);
        System.setProperty("com.atomikos.icatch.threaded_2pc", "false");
        System.setProperty("com.atomikos.icatch.default_jta_timeout", "3000");
        System.setProperty("com.atomikos.icatch.max_timeout", "3000");
        System.setProperty("com.atomikos.icatch.recovery_delay", "500");
        System.setProperty("com.atomikos.icatch.checkpoint_interval", "1");
        UserTransactionManager coordinator = new UserTransactionManager();
        coordinator.init();
        KBXADataSource[] raw = new KBXADataSource[2];
        AtomikosDataSourceBean[] pools = new AtomikosDataSourceBean[2];
        KingbaseXaJdbcExecutor[] executors = new KingbaseXaJdbcExecutor[2];
        AtomicInteger committedBranches = new AtomicInteger();
        AtomicInteger preparedBranches = new AtomicInteger();
        try {
            for (int i = 0; i < raw.length; i++) {
                raw[i] = XADataSourceUtil.createKingbase(args[5 + i],
                    System.getenv().getOrDefault("KINGBASE_XA_TEST_USERNAME", "system"),
                    System.getenv().getOrDefault("KINGBASE_XA_TEST_PASSWORD", "1234"));
                ObservedXADataSource observed = new ObservedXADataSource(raw[i]);
                if (stage.equals("crash-after-first-commit")) {
                    observed.afterCommit = () -> {
                        if (committedBranches.incrementAndGet() == 1) {
                            System.out.println("HALT: one branch committed after durable COMMIT decision");
                            System.out.flush();
                            Runtime.getRuntime().halt(73);
                        }
                    };
                } else if (stage.equals("crash-after-prepare")) {
                    observed.afterPrepare = () -> {
                        if (preparedBranches.incrementAndGet() == 2) {
                            System.out.println("HALT: both branches prepared before COMMIT decision");
                            System.out.flush();
                            Runtime.getRuntime().halt(74);
                        }
                    };
                }
                pools[i] = XADataSourceUtil.managed("recovery-" + i, observed, 1);
                pools[i].init();
                executors[i] = new KingbaseXaJdbcExecutor(pools[i]);
            }
            if (stage.startsWith("crash-")) {
                TransactionTemplate transaction = new TransactionTemplate(
                    new JtaTransactionManager(new UserTransactionImp(), coordinator));
                transaction.setTimeout(3);
                transaction.executeWithoutResult(status -> {
                    for (KingbaseXaJdbcExecutor executor : executors) {
                        executor.update(SqlRequests.of("INSERT INTO " + table + " VALUES (?, ?)", id, "crash-recovery"));
                    }
                });
                throw new AssertionError("The requested XA crash point was not reached");
            }

            int expectedRows = stage.equals("recover-commit") ? 1 : 0;
            long deadline = System.nanoTime() + java.util.concurrent.TimeUnit.SECONDS.toNanos(20);
            while (System.nanoTime() < deadline) {
                Configuration.getRecoveryService().performRecovery();
                boolean recovered = true;
                for (KBXADataSource source : raw) {
                    try (Connection connection = source.getConnection(); var statement = connection.createStatement()) {
                        try (var pending = statement.executeQuery("SELECT count(*) FROM pg_prepared_xacts")) {
                            pending.next();
                            recovered &= pending.getInt(1) == 0;
                        }
                        try (var count = connection.prepareStatement("SELECT count(*) FROM " + table + " WHERE id = ?")) {
                            count.setString(1, id);
                            try (var rows = count.executeQuery()) {
                                rows.next();
                                recovered &= rows.getInt(1) == expectedRows;
                            }
                        }
                    }
                }
                if (recovered) {
                    System.out.println("RECOVERED: expected rows per database=" + expectedRows);
                    return;
                }
                Thread.sleep(250);
            }
            throw new AssertionError("Recovery did not finish within 20 seconds");
        } finally {
            for (AtomikosDataSourceBean pool : pools) if (pool != null) pool.close();
            coordinator.close();
        }
    }
}
