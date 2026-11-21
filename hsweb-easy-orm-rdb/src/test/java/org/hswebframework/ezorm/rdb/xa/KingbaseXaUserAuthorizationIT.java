package org.hswebframework.ezorm.rdb.xa;

import com.atomikos.icatch.jta.UserTransactionImp;
import com.atomikos.icatch.jta.UserTransactionManager;
import com.atomikos.jdbc.AtomikosDataSourceBean;
import com.kingbase8.xa.KBXADataSource;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.transaction.UnexpectedRollbackException;
import org.springframework.transaction.jta.JtaTransactionManager;
import org.springframework.transaction.support.TransactionTemplate;

import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Map;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;

/** Run with JUnit; @Before creates tables and @After closes resources without deleting data. */
public class KingbaseXaUserAuthorizationIT {

    private static final String EXISTING_AUTHORIZATION_ID = "xa-auth-existing";

    private String userUrl;
    private String authorizationUrl;
    private UserTransactionManager coordinator;
    private AtomikosDataSourceBean userPool;
    private AtomikosDataSourceBean authorizationPool;
    private KingbaseXaJdbcExecutor userExecutor;
    private KingbaseXaJdbcExecutor authorizationExecutor;
    private TransactionTemplate transaction;
    private Map<String, Object> originalAuthorization;

    @Before
    public void setUp() throws Exception {
        String username = System.getProperty("kingbase.xa.username", "system");
        String password = System.getProperty("kingbase.xa.password",
            System.getenv().getOrDefault("KINGBASE_XA_TEST_PASSWORD", "1234"));
        userUrl = System.getProperty("kingbase.xa.mysql.url", "jdbc:kingbase8://127.0.0.1:54321/test");
        authorizationUrl = System.getProperty("kingbase.xa.oracle.url", "jdbc:kingbase8://127.0.0.1:54322/test");
        KBXADataSource users = XADataSourceUtil.createKingbase(userUrl, username, password);
        KBXADataSource authorizations = XADataSourceUtil.createKingbase(authorizationUrl, username, password);

        prepare(users, "mysql", "CREATE TABLE IF NOT EXISTS public.\"xa_demo_user\" "
            + "(id varchar(64) PRIMARY KEY, username varchar(64) NOT NULL)");
        prepare(authorizations, "oracle", "CREATE TABLE IF NOT EXISTS public.\"xa_demo_authorization\" "
            + "(id varchar(64) PRIMARY KEY, user_id varchar(64) NOT NULL, permission varchar(64) NOT NULL)");

        Path logs = Path.of(System.getProperty("kingbase.xa.demo.logDir", "target/xa-user-auth-demo")).toAbsolutePath();
        Files.createDirectories(logs);
        System.setProperty("com.atomikos.icatch.log_base_dir", logs.toString());
        System.setProperty("com.atomikos.icatch.tm_unique_name", "xa-user-auth-demo");
        coordinator = new UserTransactionManager();
        coordinator.init();
        userPool = XADataSourceUtil.managed("xa-demo-user", users, 2);
        authorizationPool = XADataSourceUtil.managed("xa-demo-authorization", authorizations, 2);
        userPool.init();
        authorizationPool.init();
        userExecutor = new KingbaseXaJdbcExecutor(userPool);
        authorizationExecutor = new KingbaseXaJdbcExecutor(authorizationPool);
        transaction = new TransactionTemplate(new JtaTransactionManager(new UserTransactionImp(), coordinator));
        transaction.setTimeout(30);

        // Prepare and commit the existing authorization outside the tested XA transaction.
        originalAuthorization = findAuthorization(EXISTING_AUTHORIZATION_ID);
        if (originalAuthorization == null) {
            authorizationExecutor.update(SqlRequests.of(
                "INSERT INTO public.\"xa_demo_authorization\" (id, user_id, permission) VALUES (?, ?, ?)",
                EXISTING_AUTHORIZATION_ID, "xa-existing-user", "user:query"));
            originalAuthorization = findAuthorization(EXISTING_AUTHORIZATION_ID);
        }
        assertNotNull(originalAuthorization);
    }

    @Test
    public void createUserAndAuthorizationAcrossDatabases() {
        String suffix = UUID.randomUUID().toString().replace("-", "").substring(0, 12);
        String userId = "xa-user-" + suffix;
        String authorizationId = "xa-auth-" + suffix;
        String userName = "xa_demo_" + suffix;

        transaction.executeWithoutResult(status -> {
            userExecutor.update(SqlRequests.of(
                "INSERT INTO public.\"xa_demo_user\" (id, username) VALUES (?, ?)", userId, userName));
            authorizationExecutor.update(SqlRequests.of(
                "INSERT INTO public.\"xa_demo_authorization\" (id, user_id, permission) VALUES (?, ?, ?)",
                authorizationId, userId, "user:query"));
        });

        var user = userExecutor.select(SqlRequests.of(
            "SELECT id, username FROM public.\"xa_demo_user\" WHERE id = ?", userId),
            ResultWrappers.lowerCase(ResultWrappers.singleMap()));
        var authorization = authorizationExecutor.select(SqlRequests.of(
            "SELECT id, user_id, permission FROM public.\"xa_demo_authorization\" WHERE id = ?", authorizationId),
            ResultWrappers.lowerCase(ResultWrappers.singleMap()));
        assertNotNull(user);
        assertNotNull(authorization);
        assertEquals(userName, user.get("username"));
        assertEquals(userId, authorization.get("user_id"));
        assertEquals("user:query", authorization.get("permission"));

        System.out.println("XA事务提交成功，演示表和数据已保留。");
        System.out.println("用户库：" + userUrl);
        System.out.println("用户记录：" + user);
        System.out.println("SELECT id, username FROM public.\"xa_demo_user\" WHERE id = '" + userId + "';");
        System.out.println("授权库：" + authorizationUrl);
        System.out.println("授权记录：" + authorization);
        System.out.println("SELECT id, user_id, permission FROM public.\"xa_demo_authorization\" WHERE user_id = '" + userId + "';");
    }

    @Test
    public void duplicateAuthorizationRollsBackUser() {
        String userId = "xa-conflict-" + UUID.randomUUID();

        Throwable failure = assertThrows(Throwable.class, () -> transaction.executeWithoutResult(status -> {
            insertUser(userId);
            insertAuthorization(EXISTING_AUTHORIZATION_ID, userId);
        }));

        SQLException databaseError = null;
        for (Throwable cause = failure; cause != null; cause = cause.getCause()) {
            if (cause instanceof SQLException) databaseError = (SQLException) cause;
        }
        assertNotNull("必须由真实数据库的主键冲突触发回滚", databaseError);
        assertEquals("23505", databaseError.getSQLState());
        assertRolledBack("授权主键冲突", userId);
    }

    @Test
    public void businessExceptionRollsBackUserAndAuthorization() {
        String userId = "xa-business-" + UUID.randomUUID();
        String authorizationId = "xa-business-auth-" + UUID.randomUUID();

        IllegalStateException failure = assertThrows(IllegalStateException.class,
            () -> transaction.executeWithoutResult(status -> {
                insertUser(userId);
                insertAuthorization(authorizationId, userId);
                throw new IllegalStateException("模拟用户和授权写入后的业务异常");
            }));

        assertEquals("模拟用户和授权写入后的业务异常", failure.getMessage());
        assertNull(findAuthorization(authorizationId));
        assertRolledBack("业务处理异常", userId);
    }

    @Test
    public void transactionTimeoutRollsBackUserAndAuthorization() {
        String userId = "xa-timeout-" + UUID.randomUUID();
        String authorizationId = "xa-timeout-auth-" + UUID.randomUUID();
        TransactionTemplate timed = new TransactionTemplate(transaction.getTransactionManager());
        timed.setTimeout(1);

        assertThrows(UnexpectedRollbackException.class, () -> timed.executeWithoutResult(status -> {
            insertUser(userId);
            insertAuthorization(authorizationId, userId);
            try {
                // Exceed the JTA deadline; let the coordinator detect and roll back the transaction.
                Thread.sleep(2500);
            } catch (InterruptedException error) {
                Thread.currentThread().interrupt();
                throw new IllegalStateException(error);
            }
        }));

        assertNull(findAuthorization(authorizationId));
        assertRolledBack("事务超时", userId);
    }

    private void insertUser(String userId) {
        assertEquals(1, userExecutor.update(SqlRequests.of(
            "INSERT INTO public.\"xa_demo_user\" (id, username) VALUES (?, ?)", userId, "xa_rollback_demo")));
    }

    private void insertAuthorization(String authorizationId, String userId) {
        assertEquals(1, authorizationExecutor.update(SqlRequests.of(
            "INSERT INTO public.\"xa_demo_authorization\" (id, user_id, permission) VALUES (?, ?, ?)",
            authorizationId, userId, "user:query")));
    }

    private Map<String, Object> findAuthorization(String authorizationId) {
        return authorizationExecutor.select(SqlRequests.of(
            "SELECT id, user_id, permission FROM public.\"xa_demo_authorization\" WHERE id = ?", authorizationId),
            ResultWrappers.lowerCase(ResultWrappers.singleMap()));
    }

    private void assertRolledBack(String scenario, String userId) {
        assertNull("用户库不应保留本次新增用户", userExecutor.select(SqlRequests.of(
            "SELECT id, username FROM public.\"xa_demo_user\" WHERE id = ?", userId),
            ResultWrappers.lowerCase(ResultWrappers.singleMap())));
        assertNull("授权库不应保留本次新增授权", authorizationExecutor.select(SqlRequests.of(
            "SELECT id FROM public.\"xa_demo_authorization\" WHERE user_id = ?", userId),
            ResultWrappers.lowerCase(ResultWrappers.singleMap())));
        assertEquals("原有授权记录必须保持不变", originalAuthorization, findAuthorization(EXISTING_AUTHORIZATION_ID));

        System.out.println(scenario + "：已回滚本次新增数据，原有授权记录保持不变。");
        System.out.println("用户库：" + userUrl);
        System.out.println("SELECT * FROM public.\"xa_demo_user\" WHERE id = '" + userId + "';");
        System.out.println("授权库：" + authorizationUrl);
        System.out.println("SELECT * FROM public.\"xa_demo_authorization\" WHERE user_id = '" + userId + "';");
        System.out.println("SELECT * FROM public.\"xa_demo_authorization\" WHERE id = '" + EXISTING_AUTHORIZATION_ID + "';");
    }

    @After
    public void tearDown() {
        try {
            if (authorizationPool != null) authorizationPool.close();
        } finally {
            try {
                if (userPool != null) userPool.close();
            } finally {
                if (coordinator != null) coordinator.close();
            }
        }
    }

    private static void prepare(KBXADataSource source, String expectedMode, String ddl) throws Exception {
        try (Connection connection = source.getConnection(); var statement = connection.createStatement()) {
            try (var mode = statement.executeQuery("SHOW database_mode")) {
                if (!mode.next() || !expectedMode.equalsIgnoreCase(mode.getString(1))) {
                    throw new IllegalArgumentException("Expected Kingbase mode " + expectedMode + ": " + source.getUrl());
                }
            }
            try (var capacity = statement.executeQuery("SHOW max_prepared_transactions")) {
                if (!capacity.next() || capacity.getInt(1) < 1) {
                    throw new IllegalStateException("XA requires max_prepared_transactions > 0: " + source.getUrl());
                }
            }
            statement.execute(ddl);
        }
    }
}
