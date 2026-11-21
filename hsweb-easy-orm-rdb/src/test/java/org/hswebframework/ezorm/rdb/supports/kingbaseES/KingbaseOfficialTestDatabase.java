package org.hswebframework.ezorm.rdb.supports.kingbaseES;

import io.r2dbc.spi.Connection;
import io.r2dbc.spi.ConnectionFactories;
import io.r2dbc.spi.ConnectionFactory;
import io.r2dbc.spi.ConnectionFactoryOptions;
import io.r2dbc.spi.Option;
import org.hswebframework.ezorm.rdb.R2dbcConnectionProvider;
import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.kingbaseES.mysql.KingbaseMysqlSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.kingbaseES.oracle.KingbaseOracleSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.kingbaseES.mssql.KingbaseSqlServerSchemaMetadata;
import org.junit.Assume;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.Locale;
import java.util.UUID;
import java.util.function.Function;

/** Connection and isolated schema for the upstream-derived test suites. */
public class KingbaseOfficialTestDatabase extends KingbaseTestContainer {

    private static final Duration TIMEOUT = Duration.ofSeconds(30);

    private final String mode;

    private ConnectionFactory adminFactory;
    private ReactiveSqlExecutor executor;
    private String schemaName;

    public KingbaseOfficialTestDatabase(String mode) {
        super(mode);
        this.mode = mode;
    }

    @Override
    protected void before() {
        super.before();
        try {
            Assume.assumeTrue("Enable Kingbase " + mode + " integration tests",
                Boolean.getBoolean("kingbase." + mode + ".r2dbc.integration")
                    || Boolean.getBoolean("kingbase." + mode + ".r2dbc.enabled"));
            ConnectionFactoryOptions.Builder options = ConnectionFactoryOptions.parse(configuration(
                    "url", "r2dbc:kingbase://127.0.0.1:" + ("mysql".equals(mode) ? 54321 : "sqlserver".equals(mode) ? 54323 : 54322)
                        + "/test?mode=" + mode))
                .mutate()
                .option(ConnectionFactoryOptions.USER, configuration("username", "system"))
                .option(ConnectionFactoryOptions.PASSWORD, configuration("password", "1234"));
            adminFactory = ConnectionFactories.get(options.build());
            schemaName = "ez_kb_official_" + UUID.randomUUID().toString().replace("-", "");
            executeAdmin("create schema " + newSchema().getDialect().quote(schemaName, false));
            ConnectionFactory factory = ConnectionFactories.get(
                options.option(Option.valueOf("schema"), schemaName).build());
            R2dbcConnectionProvider provider = new R2dbcConnectionProvider() {
                @Override
                public Mono<Connection> getConnection() {
                    return Mono.defer(() -> Mono.from(factory.create()));
                }

                @Override
                public void releaseConnection(Connection connection) {
                    // The executor closes the connection asynchronously.
                }
            };
            executor = new TestReactiveSqlExecutor(provider) {
                @Override
                protected <T> Flux<T> doInConnection(Function<Connection, Publisher<T>> handler) {
                    return Flux.usingWhen(provider.getConnection(), handler,
                        connection -> Mono.from(connection.close()),
                        (connection, error) -> Mono.from(connection.close()),
                        connection -> Mono.from(connection.close()));
                }
            };
        } catch (RuntimeException | Error error) {
            after();
            throw error;
        }
    }

    @Override
    protected void after() {
        try {
            if (schemaName != null && adminFactory != null) {
                executeAdmin("drop schema if exists " + newSchema().getDialect().quote(schemaName, false) + " cascade");
            }
        } finally {
            schemaName = null;
            super.after();
        }
    }

    public RDBSchemaMetadata newSchema() {
        if ("sqlserver".equals(mode)) {
            return new KingbaseSqlServerSchemaMetadata(schemaName);
        }
        return "mysql".equals(mode) ? new KingbaseMysqlSchemaMetadata(schemaName)
            : new KingbaseOracleSchemaMetadata(schemaName);
    }

    public ReactiveSqlExecutor reactive() {
        return executor;
    }

    public SyncSqlExecutor sync() {
        return ReactiveSyncSqlExecutor.of(executor);
    }

    private void executeAdmin(String sql) {
        Mono.usingWhen(Mono.from(adminFactory.create()),
            connection -> Flux.from(connection.createStatement(sql).execute())
                .concatMap(result -> result.getRowsUpdated()).then(),
            connection -> Mono.from(connection.close()))
            .block(TIMEOUT);
    }

    private String configuration(String name, String defaultValue) {
        String value = System.getProperty("kingbase." + mode + ".r2dbc." + name);
        if (value == null || value.isEmpty()) {
            value = System.getenv(("KINGBASE_" + mode + "_R2DBC_" + name).toUpperCase(Locale.ROOT));
        }
        return value == null || value.isEmpty() ? defaultValue : value;
    }
}
