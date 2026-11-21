package org.hswebframework.ezorm.rdb.xa;

import com.atomikos.icatch.jta.UserTransactionImp;
import com.atomikos.icatch.jta.UserTransactionManager;
import com.atomikos.jdbc.AtomikosDataSourceBean;
import com.kingbase8.xa.KBXADataSource;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.supports.kingbaseES.mysql.KingbaseMysqlSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.kingbaseES.oracle.KingbaseOracleSchemaMetadata;
import org.junit.AfterClass;
import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.transaction.jta.JtaTransactionManager;
import org.springframework.transaction.support.TransactionTemplate;

import java.nio.file.Files;
import java.sql.Connection;
import java.sql.ResultSet;
import java.util.UUID;

import static org.junit.Assert.*;

/** Real JDBC XA transactions across two independent Kingbase compatibility modes. */
public class KingbaseJdbcXaIT {

    private static final String TABLE = "xa_it_" + UUID.randomUUID().toString().replace("-", "").substring(0, 12);
    private static final String SQL_TABLE = "\"" + TABLE + "\"";
    private static final KBXADataSource[] raw = new KBXADataSource[2];
    private static final AtomikosDataSourceBean[] pools = new AtomikosDataSourceBean[2];
    private static final ObservedXADataSource[] observed = new ObservedXADataSource[2];
    private static final KingbaseXaJdbcExecutor[] executors = new KingbaseXaJdbcExecutor[2];
    private static UserTransactionManager coordinator;
    private static TransactionTemplate transactions;

    @BeforeClass
    public static void start() throws Exception {
        String logDirectory = Files.createTempDirectory("hsweb-xa-jta-").toString();
        System.setProperty("com.atomikos.icatch.log_base_dir", logDirectory);
        System.setProperty("com.atomikos.icatch.tm_unique_name", "hswebxa-" + UUID.randomUUID().toString().substring(0, 8));
        System.setProperty("com.atomikos.icatch.threaded_2pc", "false");
        System.setProperty("com.atomikos.icatch.default_jta_timeout", "10000");
        System.setProperty("com.atomikos.icatch.recovery_delay", "1000");
        coordinator = new UserTransactionManager();
        coordinator.init();
        transactions = new TransactionTemplate(new JtaTransactionManager(new UserTransactionImp(), coordinator));
        transactions.setTimeout(10);
        String username = System.getProperty("kingbase.xa.username", "system");
        String password = System.getProperty("kingbase.xa.password",
            System.getenv().getOrDefault("KINGBASE_XA_TEST_PASSWORD", "1234"));
        String[] urls = {
            System.getProperty("kingbase.xa.mysql.url", "jdbc:kingbase8://127.0.0.1:54321/test"),
            System.getProperty("kingbase.xa.oracle.url", "jdbc:kingbase8://127.0.0.1:54322/test")
        };
        String[] expectedModes = {"mysql", "oracle"};
        for (int i = 0; i < raw.length; i++) {
            raw[i] = XADataSourceUtil.createKingbase(urls[i], username, password);
            try (Connection connection = raw[i].getConnection(); var statement = connection.createStatement()) {
                try (ResultSet mode = statement.executeQuery("SHOW database_mode")) {
                    assertTrue(mode.next());
                    assertEquals(expectedModes[i], mode.getString(1).toLowerCase());
                }
                try (ResultSet capacity = statement.executeQuery("SHOW max_prepared_transactions")) {
                    assertTrue(capacity.next());
                    assertTrue("XA requires max_prepared_transactions > 0", capacity.getInt(1) > 0);
                }
                statement.execute("CREATE TABLE " + SQL_TABLE + " (id varchar(64) PRIMARY KEY, name varchar(64))");
            }
            observed[i] = new ObservedXADataSource(raw[i]);
            pools[i] = XADataSourceUtil.managed("hsweb-xa-" + expectedModes[i], observed[i], 2);
            pools[i].init();
            executors[i] = new KingbaseXaJdbcExecutor(pools[i]);
        }
    }

    @AfterClass
    public static void stop() throws Exception {
        for (AtomikosDataSourceBean pool : pools) {
            if (pool != null) pool.close();
        }
        if (coordinator != null) coordinator.close();
        for (KBXADataSource source : raw) {
            if (source == null) continue;
            try (Connection connection = source.getConnection(); var statement = connection.createStatement()) {
                statement.execute("DROP TABLE IF EXISTS " + SQL_TABLE);
            }
        }
    }

    @After
    public void leavesNoPreparedTransactionsOrBorrowedConnections() {
        assertPoolsReturned();
        for (KingbaseXaJdbcExecutor executor : executors) {
            int prepared = ((Number) executor.select(SqlRequests.of("SELECT count(*) AS total FROM pg_prepared_xacts"),
                ResultWrappers.lowerCase(ResultWrappers.singleMap())).get("total")).intValue();
            assertEquals("An isolated test database must have no unresolved XA branches", 0, prepared);
        }
        assertPoolsReturned();
    }

    @Test
    public void commitsBothCompatibilityModesInOneGlobalTransaction() {
        String id = UUID.randomUUID().toString();
        int preparesBefore = observed[0].prepares.get() + observed[1].prepares.get();
        int commitsBefore = observed[0].twoPhaseCommits.get() + observed[1].twoPhaseCommits.get();
        transactions.executeWithoutResult(status -> {
            for (KingbaseXaJdbcExecutor executor : executors) {
                assertEquals(1, executor.update(SqlRequests.of(
                    "INSERT INTO " + SQL_TABLE + " (id, name) VALUES (?, ?)", id, "committed")));
            }
        });

        for (KingbaseXaJdbcExecutor executor : executors) {
            var record = executor.select(SqlRequests.of("SELECT name FROM " + SQL_TABLE + " WHERE id = ?", id),
                ResultWrappers.lowerCase(ResultWrappers.singleMap()));
            assertEquals("committed", record.get("name"));
        }
        assertEquals("Both real XA resources must prepare", 2,
            observed[0].prepares.get() + observed[1].prepares.get() - preparesBefore);
        assertEquals("Both branches must commit with onePhase=false", 2,
            observed[0].twoPhaseCommits.get() + observed[1].twoPhaseCommits.get() - commitsBefore);
        assertPoolsReturned();
    }

    @Test
    public void rollsBackBothDatabasesWhenBusinessFailsAfterBothWrites() {
        String id = UUID.randomUUID().toString();
        IllegalStateException failure = assertThrows(IllegalStateException.class,
            () -> transactions.executeWithoutResult(status -> {
                for (KingbaseXaJdbcExecutor executor : executors) {
                    executor.update(SqlRequests.of("INSERT INTO " + SQL_TABLE + " VALUES (?, ?)", id, "rollback"));
                }
                throw new IllegalStateException("business failed after both writes");
            }));
        assertEquals("business failed after both writes", failure.getMessage());
        for (int i = 0; i < raw.length; i++) assertEquals(0, count(i, id));
        assertPoolsReturned();
    }

    @Test
    public void databaseConflictRollsBackEarlierWriteInTheOtherMode() throws Exception {
        String id = UUID.randomUUID().toString();
        try (Connection connection = raw[1].getConnection(); var insert = connection.prepareStatement(
            "INSERT INTO " + SQL_TABLE + " VALUES (?, ?)")) {
            insert.setString(1, id);
            insert.setString(2, "existing");
            insert.executeUpdate();
        }

        Throwable failure = assertThrows(Throwable.class, () -> transactions.executeWithoutResult(status -> {
            executors[0].update(SqlRequests.of("INSERT INTO " + SQL_TABLE + " VALUES (?, ?)", id, "must-rollback"));
            executors[1].update(SqlRequests.of("INSERT INTO " + SQL_TABLE + " VALUES (?, ?)", id, "duplicate"));
        }));
        java.sql.SQLException databaseError = null;
        for (Throwable cause = failure; cause != null; cause = cause.getCause()) {
            if (cause instanceof java.sql.SQLException) databaseError = (java.sql.SQLException) cause;
        }
        assertNotNull("The error must come from the real JDBC database operation", databaseError);
        assertEquals("23505", databaseError.getSQLState());
        assertEquals(0, count(0, id));
        assertEquals(1, count(1, id));
        assertEquals("existing", executors[1].select(SqlRequests.of(
            "SELECT name FROM " + SQL_TABLE + " WHERE id = ?", id), ResultWrappers.lowerCase(ResultWrappers.singleMap())).get("name"));
        assertPoolsReturned();
    }

    @Test
    public void failedPrepareVotePreventsBothCommits() {
        String id = UUID.randomUUID().toString();
        observed[1].rejectNextPrepare.set(true);
        try {
            assertThrows(org.springframework.transaction.UnexpectedRollbackException.class,
                () -> transactions.executeWithoutResult(status -> {
                    for (KingbaseXaJdbcExecutor executor : executors) {
                        executor.update(SqlRequests.of("INSERT INTO " + SQL_TABLE + " VALUES (?, ?)", id, "prepare-rejected"));
                    }
                }));
        } finally {
            observed[1].rejectNextPrepare.set(false);
        }
        assertEquals(0, count(0, id));
        assertEquals(0, count(1, id));
        assertPoolsReturned();
    }

    @Test
    public void executeDoesNotSuspendTheGlobalTransaction() {
        String id = UUID.randomUUID().toString();
        assertThrows(IllegalStateException.class, () -> transactions.executeWithoutResult(status -> {
            for (KingbaseXaJdbcExecutor executor : executors) {
                executor.execute(SqlRequests.of("INSERT INTO " + SQL_TABLE + " VALUES (?, ?)", id, "execute-rollback"));
            }
            throw new IllegalStateException("execute must remain inside the JTA transaction");
        }));
        assertEquals(0, count(0, id));
        assertEquals(0, count(1, id));
    }

    @Test
    public void timedOutTransactionRollsBackBothDatabasesAndReleasesConnections() {
        String id = UUID.randomUUID().toString();
        TransactionTemplate timed = new TransactionTemplate(transactions.getTransactionManager());
        timed.setTimeout(1);
        assertThrows(org.springframework.transaction.UnexpectedRollbackException.class,
            () -> timed.executeWithoutResult(status -> {
                for (KingbaseXaJdbcExecutor executor : executors) {
                    executor.update(SqlRequests.of("INSERT INTO " + SQL_TABLE + " VALUES (?, ?)", id, "timeout"));
                }
                try {
                    Thread.sleep(1500);
                } catch (InterruptedException error) {
                    Thread.currentThread().interrupt();
                    throw new IllegalStateException(error);
                }
            }));
        assertEquals(0, count(0, id));
        assertEquals(0, count(1, id));
    }

    @Test
    public void reusesConnectionsWithinTransactionAndReleasesThemAfterCompletion() {
        for (int iteration = 0; iteration < 5; iteration++) {
            transactions.executeWithoutResult(status -> {
                for (KingbaseXaJdbcExecutor executor : executors) {
                    var request = SqlRequests.of("SELECT 1");
                    Connection first = executor.getConnection(request);
                    Connection second = executor.getConnection(request);
                    try {
                        assertSame("Repeated ORM calls must reuse the transaction-bound connection", first, second);
                        assertEquals(1, ((Number) executor.select(SqlRequests.of("SELECT 1 AS value"),
                            ResultWrappers.lowerCase(ResultWrappers.singleMap())).get("value")).intValue());
                    } finally {
                        executor.releaseConnection(second, request);
                        executor.releaseConnection(first, request);
                    }
                }
            });
            assertPoolsReturned();
        }
    }

    @Test
    public void ormGeneratedSqlParticipatesInTheSameXaTransaction() {
        DatabaseOperator[] databases = new DatabaseOperator[2];
        RDBSchemaMetadata[] schemas = {new KingbaseMysqlSchemaMetadata("public"), new KingbaseOracleSchemaMetadata("public")};
        for (int i = 0; i < schemas.length; i++) {
            RDBSchemaMetadata schema = schemas[i];
            RDBDatabaseMetadata metadata = new RDBDatabaseMetadata(schema.getDialect());
            metadata.setCurrentSchema(schema);
            metadata.addSchema(schema);
            metadata.addFeature(executors[i]);
            var table = schema.newTable(TABLE);
            for (String name : new String[]{"id", "name"}) {
                RDBColumnMetadata column = new RDBColumnMetadata();
                column.setName(name);
                column.setType(schema.getDialect().convertDataType("varchar(64)"));
                column.setPrimaryKey("id".equals(name));
                table.addColumn(column);
            }
            schema.addTable(table);
            databases[i] = DefaultDatabaseOperator.of(metadata);
        }

        String committed = UUID.randomUUID().toString();
        transactions.executeWithoutResult(status -> {
            for (DatabaseOperator database : databases) {
                assertEquals(1, (int) database.dml().insert(TABLE)
                    .columns("id", "name").values(committed, "orm-generated").execute().sync());
            }
        });
        for (int i = 0; i < raw.length; i++) assertEquals(1, count(i, committed));

        String rolledBack = UUID.randomUUID().toString();
        assertThrows(IllegalStateException.class, () -> transactions.executeWithoutResult(status -> {
            for (DatabaseOperator database : databases) {
                database.dml().insert(TABLE).columns("id", "name").values(rolledBack, "orm-rollback").execute().sync();
            }
            throw new IllegalStateException("rollback generated ORM writes");
        }));
        for (int i = 0; i < raw.length; i++) assertEquals(0, count(i, rolledBack));
    }

    @Test
    public void restartRecoversRemainingCommitAfterOneDatabaseAlreadyCommitted() throws Exception {
        String id = UUID.randomUUID().toString();
        String coordinatorName = "recover-" + id.substring(0, 8);
        var directory = java.nio.file.Path.of("target", "xa-recovery", id).toAbsolutePath();
        Files.createDirectories(directory.resolve("transactions"));
        try {
            assertEquals(73, runRecoveryProcess("crash-after-first-commit", directory, coordinatorName, id));
            assertEquals("Exactly one database committed before the process halted", 1, count(0, id) + count(1, id));
            assertEquals("The remaining branch is really prepared on the server", 1, preparedCount(0) + preparedCount(1));
            assertEquals(0, runRecoveryProcess("recover-commit", directory, coordinatorName, id));
            assertEquals(1, count(0, id));
            assertEquals(1, count(1, id));
        } finally {
            finishTestBranches(coordinatorName, true);
        }
    }

    @Test
    public void restartRollsBackPreparedBranchesWithoutACommitDecision() throws Exception {
        String id = UUID.randomUUID().toString();
        String coordinatorName = "abort-" + id.substring(0, 8);
        var directory = java.nio.file.Path.of("target", "xa-recovery", id).toAbsolutePath();
        Files.createDirectories(directory.resolve("transactions"));
        try {
            assertEquals(74, runRecoveryProcess("crash-after-prepare", directory, coordinatorName, id));
            assertEquals(0, count(0, id) + count(1, id));
            assertEquals("Both branches must survive the abrupt exit as prepared transactions", 2,
                preparedCount(0) + preparedCount(1));
            assertEquals(0, runRecoveryProcess("recover-rollback", directory, coordinatorName, id));
            assertEquals(0, count(0, id));
            assertEquals(0, count(1, id));
        } finally {
            finishTestBranches(coordinatorName, false);
        }
    }

    private static int preparedCount(int database) {
        return ((Number) executors[database].select(SqlRequests.of("SELECT count(*) AS total FROM pg_prepared_xacts"),
            ResultWrappers.lowerCase(ResultWrappers.singleMap())).get("total")).intValue();
    }

    private static int runRecoveryProcess(String stage, java.nio.file.Path directory, String coordinatorName, String id)
        throws Exception {
        String classPath = System.getProperty("surefire.test.class.path", System.getProperty("java.class.path"));
        var command = java.util.List.of(java.nio.file.Path.of(System.getProperty("java.home"), "bin", "java").toString(),
            "-cp", classPath, XaRecoveryProcess.class.getName(), stage, directory.resolve("transactions").toString(),
            coordinatorName, TABLE, id, raw[0].getUrl(), raw[1].getUrl());
        var output = directory.resolve(stage + ".log");
        ProcessBuilder builder = new ProcessBuilder(command).redirectErrorStream(true).redirectOutput(output.toFile());
        builder.environment().put("KINGBASE_XA_TEST_USERNAME", raw[0].getUser());
        builder.environment().put("KINGBASE_XA_TEST_PASSWORD", raw[0].getPassword());
        Process process = builder.start();
        if (!process.waitFor(35, java.util.concurrent.TimeUnit.SECONDS)) {
            process.destroyForcibly().waitFor();
            fail("Recovery process timed out: " + output);
        }
        if (process.exitValue() != 0 && process.exitValue() != 73 && process.exitValue() != 74) {
            fail("Recovery subprocess failed; inspect " + output + "\n" + Files.readString(output));
        }
        return process.exitValue();
    }

    /** Failure cleanup only: completes this test's own XIDs without masking any assertion failure. */
    private static void finishTestBranches(String coordinatorName, boolean commit) throws Exception {
        for (KBXADataSource source : raw) {
            var connection = source.getXAConnection();
            try {
                var resource = connection.getXAResource();
                var pending = resource.recover(javax.transaction.xa.XAResource.TMSTARTRSCAN);
                for (var xid : pending) {
                    if (new String(xid.getGlobalTransactionId(), java.nio.charset.StandardCharsets.UTF_8).startsWith(coordinatorName)) {
                        if (commit) resource.commit(xid, false);
                        else resource.rollback(xid);
                    }
                }
                resource.recover(javax.transaction.xa.XAResource.TMENDRSCAN);
            } finally {
                connection.close();
            }
        }
    }

    private static int count(int database, String id) {
        return ((Number) executors[database].select(SqlRequests.of(
            "SELECT count(*) AS total FROM " + SQL_TABLE + " WHERE id = ?", id),
            ResultWrappers.lowerCase(ResultWrappers.singleMap())).get("total")).intValue();
    }

    private static void assertPoolsReturned() {
        for (AtomikosDataSourceBean pool : pools) {
            assertEquals("All borrowed connections must return to the pool", pool.poolTotalSize(), pool.poolAvailableSize());
        }
    }
}
