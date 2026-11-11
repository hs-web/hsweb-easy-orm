package org.hswebframework.ezorm.rdb.supports.json;

import io.r2dbc.spi.ConnectionFactories;
import io.r2dbc.spi.ConnectionFactoryOptions;
import io.r2dbc.spi.Statement;
import org.hswebframework.ezorm.rdb.ConnectionProvider;
import org.hswebframework.ezorm.rdb.R2dbcConnectionProvider;
import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.insert.MultiInsertSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperator;
import org.hswebframework.ezorm.rdb.supports.mysql.Mysql8ConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.postgres.JsonbType;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlSchemaMetadata;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import reactor.core.publisher.Mono;

import java.sql.Connection;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.UUID;

import static io.r2dbc.spi.ConnectionFactoryOptions.PASSWORD;
import static io.r2dbc.spi.ConnectionFactoryOptions.USER;
import static org.junit.Assert.*;

/** Native JSON writes through both JDBC and R2DBC, sharing the existing database test providers. */
@RunWith(Parameterized.class)
public class NativeJsonWriteIntegrationTest {
    @Parameterized.Parameters(name = "{0}-{1}")
    public static Collection<Object[]> databases() {
        return Arrays.asList(new Object[][] {
            {"postgres", org.hswebframework.ezorm.rdb.supports.postgres.JsonType.INSTANCE},
            {"postgres", JsonbType.INSTANCE},
            {"mysql8", JsonType.INSTANCE}
        });
    }

    private final String driver;
    private final DataType jsonType;
    private final String table = "json_write_" + UUID.randomUUID().toString().replace("-", "");
    private DatabaseOperator database;
    private Connection jdbc;
    private io.r2dbc.spi.Connection reactive;

    public NativeJsonWriteIntegrationTest(String driver, DataType jsonType) {
        this.driver = driver;
        this.jsonType = jsonType;
    }

    @Before
    public void prepare() throws Exception {
        boolean postgres = "postgres".equals(driver);
        ConnectionProvider provider = postgres ? new PostgresqlConnectionProvider() : new Mysql8ConnectionProvider();
        jdbc = provider.getConnection();
        String jdbcUrl = jdbc.getMetaData().getURL().split("\\?", 2)[0];
        ConnectionFactoryOptions options = ConnectionFactoryOptions
            .parse("r2dbc:" + jdbcUrl.substring("jdbc:".length()))
            .mutate()
            .option(USER, System.getProperty(driver + ".username", postgres ? "postgres" : "root"))
            .option(PASSWORD, System.getProperty(driver + ".password", postgres ? "admin" : "root"))
            .build();
        reactive = Mono.from(ConnectionFactories.get(options).create()).block();
        RDBDatabaseMetadata metadata = new RDBDatabaseMetadata(postgres ? Dialect.POSTGRES : Dialect.MYSQL);
        RDBSchemaMetadata schema = postgres
            ? new PostgresqlSchemaMetadata("public")
            : new MysqlSchemaMetadata(jdbc.getCatalog());
        metadata.addSchema(schema);
        metadata.setCurrentSchema(schema);
        // Test connections are owned here and closed once per test, not once per statement.
        metadata.addFeature(new TestSyncSqlExecutor(new ConnectionProvider() {
            @Override public Connection getConnection() { return jdbc; }
            @Override public void releaseConnect(Connection connection) { }
        }));
        metadata.addFeature(new TestReactiveSqlExecutor(new R2dbcConnectionProvider() {
            @Override public Mono<io.r2dbc.spi.Connection> getConnection() { return Mono.just(reactive); }
            @Override public void releaseConnection(io.r2dbc.spi.Connection connection) { }
        }) {
            @Override protected SqlRequest convertRequest(SqlRequest request) {
                return postgres ? super.convertRequest(request) : request;
            }
            @Override protected void bind(Statement statement, int index, Object value) {
                if (postgres) { super.bind(statement, index, value); }
                else { statement.bind(index, value); }
            }
            @Override protected void bindNull(Statement statement, int index, Class<?> type) {
                if (postgres) { super.bindNull(statement, index, type); }
                else { statement.bindNull(index, type); }
            }
        });
        database = DefaultDatabaseOperator.of(metadata);
        database.ddl().createOrAlter(table)
                .addColumn().name("id").varchar(32).primaryKey().commit()
                .addColumn().name("data").type(jsonType).commit()
                .commit().sync();
    }

    @After
    public void close() throws Exception {
        try {
            if (database != null) {
                database.sql().sync().execute(SqlRequests.of("drop table if exists " + table));
            }
        } finally {
            try {
                if (reactive != null) { Mono.from(reactive.close()).block(); }
            } finally {
                if (jdbc != null) { jdbc.close(); }
            }
        }
    }

    @Test
    public void syncUpdateUsesCodecGeneratedExpression() {
        database.dml().insert(table).columns("id", "data").values("one", "{}").execute().sync();
        database.dml().update(table).set("data", Collections.singletonMap("name", "updated"))
                .where(q -> q.and("id", "one")).execute().sync();
        assertTrue(String.valueOf(row("one").get("data")).contains("updated"));
    }

    @Test
    public void reactiveUpdateUsesCodecGeneratedExpression() {
        database.dml().insert(table).columns("id", "data").values("one", "{}").execute().reactive().block();
        database.dml().update(table).set("data", Collections.singletonMap("name", "updated"))
                .where(q -> q.and("id", "one")).execute().reactive().block();
        assertTrue(String.valueOf(row("one").get("data")).contains("updated"));
    }

    @Test
    public void syncBatchDistinguishesSqlNullFromJsonNull() {
        database.dml().insert(table).columns("id", "data")
                .values("sql-null", null).values("object", Collections.singletonMap("id", "object"))
                .values("json-null", "null").values("last-null", null).execute().sync();
        assertBatch();
        database.dml().update(table).set("data", NullValue.of(jsonType))
                .where(q -> q.and("id", "object")).execute().sync();
        assertNull(row("object").get("data"));
    }

    @Test
    public void reactiveBatchDistinguishesSqlNullFromJsonNull() {
        database.dml().insert(table).columns("id", "data")
                .values("sql-null", null).values("object", Collections.singletonMap("id", "object"))
                .values("json-null", "null").values("last-null", null).execute().reactive().block();
        assertBatch();
        database.dml().update(table).set("data", NullValue.of(jsonType))
                .where(q -> q.and("id", "object")).execute().reactive().block();
        assertNull(row("object").get("data"));
    }

    @Test
    public void multiInsertUsesEncodedExpressionsInBothExecutors() {
        RDBTableMetadata metadata = database.getMetadata().getTable(table).orElseThrow(AssertionError::new);
        metadata.addFeature(MultiInsertSqlBuilder.of(metadata));
        InsertOperator insert = database.dml().insert(table).columns("id", "data")
            .values("sql-null", null).values("object", Collections.singletonMap("id", "object"))
            .values("json-null", "null").values("last-null", null);
        insert.execute().sync();
        assertBatch();
        database.dml().delete(table).where(q -> q.notNull("id")).execute().sync();
        insert.execute().reactive().block();
        assertBatch();
    }

    private void assertBatch() {
        assertNull(row("sql-null").get("data"));
        assertTrue(String.valueOf(row("object").get("data")).contains("object"));
        assertEquals("null", row("json-null").get("data"));
        assertNull(row("last-null").get("data"));
    }

    private Map<String, Object> row(String id) {
        return database.dml().query(table).where(q -> q.and("id", id)).fetch(ResultWrappers.singleMap()).sync();
    }
}
