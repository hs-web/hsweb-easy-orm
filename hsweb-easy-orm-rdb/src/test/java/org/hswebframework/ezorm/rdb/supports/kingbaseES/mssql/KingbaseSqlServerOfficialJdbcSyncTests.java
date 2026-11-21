package org.hswebframework.ezorm.rdb.supports.kingbaseES.mssql;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.ConnectionProvider;
import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.BasicCommonTests;
import org.hswebframework.ezorm.rdb.supports.kingbaseES.KingbaseOfficialTestDatabase;
import org.junit.Assume;
import org.junit.ClassRule;
import org.junit.rules.ExternalResource;
import org.junit.rules.RuleChain;
import org.junit.rules.TestRule;

import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.Statement;
import java.util.Locale;
import java.util.Properties;

/** The same 11 native common tests using the vendor JDBC driver, with no assertion overrides. */
public class KingbaseSqlServerOfficialJdbcSyncTests extends BasicCommonTests {

    private static final KingbaseOfficialTestDatabase DATABASE = new KingbaseOfficialTestDatabase("sqlserver");
    private static URLClassLoader loader;
    private static Driver driver;

    @ClassRule
    public static final TestRule RESOURCES = RuleChain.outerRule(DATABASE).around(new ExternalResource() {
        @Override
        protected void before() throws Throwable {
            String jar = configuration("jdbc.driver", null);
            Assume.assumeNotNull("Set kingbase.sqlserver.jdbc.driver to the vendor JDBC jar", jar);
            loader = new URLClassLoader(new URL[]{Paths.get(jar).toUri().toURL()}, getClass().getClassLoader());
            try {
                driver = (Driver) Class.forName("com.kingbase8.Driver", true, loader).getDeclaredConstructor().newInstance();
                try (Connection connection = provider().getConnection()) {
                    System.out.println("[Kingbase JDBC] " + connection.getMetaData().getDatabaseProductVersion()
                        + "，驱动版本=" + connection.getMetaData().getDriverVersion());
                }
            } catch (Throwable error) {
                loader.close();
                throw error;
            }
        }

        @Override
        @SneakyThrows
        protected void after() {
            driver = null;
            if (loader != null) {
                loader.close();
                loader = null;
            }
        }
    });

    @Override
    protected RDBSchemaMetadata getSchema() {
        return DATABASE.newSchema();
    }

    @Override
    protected Dialect getDialect() {
        return new KingbaseSqlServerDialect();
    }

    @Override
    protected SyncSqlExecutor getSqlExecutor() {
        return new TestSyncSqlExecutor(provider());
    }

    private static ConnectionProvider provider() {
        return new ConnectionProvider() {
            @Override
            @SneakyThrows
            public Connection getConnection() {
                Properties properties = new Properties();
                properties.setProperty("user", configuration("r2dbc.username", "system"));
                properties.setProperty("password", configuration("r2dbc.password", "1234"));
                Connection connection = driver.connect(configuration("jdbc.url", "jdbc:kingbase8://127.0.0.1:54323/test"), properties);
                if (connection == null) {
                    throw new IllegalStateException("Kingbase JDBC driver did not accept the configured URL");
                }
                try (Statement statement = connection.createStatement()) {
                    statement.execute("set search_path to " + DATABASE.newSchema().getDialect()
                        .quote(DATABASE.newSchema().getName(), false));
                } catch (Throwable error) {
                    connection.close();
                    throw error;
                }
                return connection;
            }

            @Override
            @SneakyThrows
            public void releaseConnect(Connection connection) {
                connection.close();
            }
        };
    }

    private static String configuration(String key, String fallback) {
        String value = System.getProperty("kingbase.sqlserver." + key);
        if (value == null || value.isEmpty()) {
            value = System.getenv(("KINGBASE_SQLSERVER_" + key).replace('.', '_').toUpperCase(Locale.ROOT));
        }
        return value == null || value.isEmpty() ? fallback : value;
    }
}
