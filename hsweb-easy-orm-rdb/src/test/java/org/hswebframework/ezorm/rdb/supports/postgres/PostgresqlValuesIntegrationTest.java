package org.hswebframework.ezorm.rdb.supports.postgres;

import io.r2dbc.spi.ConnectionFactories;
import org.hswebframework.ezorm.rdb.ConnectionProvider;
import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.reactive.r2dbc.R2dbcReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.junit.Assume;
import org.junit.Test;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;

import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.JDBCType;
import java.time.Duration;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.*;
import static org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlValuesTest.column;

/** 仅连接显式指定的隔离库；同一事务连接中验证值集的 JDBC/R2DBC 绑定及关联用途。 */
public class PostgresqlValuesIntegrationTest {
    private static final Duration LIMIT = Duration.ofSeconds(20);
    private static final String CREATE = "create temporary table values_probe(id varchar primary key, n numeric(38,12), b boolean, s text)";
    private static final BigDecimal PRECISE = new BigDecimal("12345678901234567890123456.123456789012");

    static SqlRequest insert() {
        List<RDBColumnMetadata> columns = List.of(column(JDBCType.VARCHAR, String.class),
            column(JDBCType.NUMERIC, BigDecimal.class), column(JDBCType.BOOLEAN, Boolean.class), column(JDBCType.LONGVARCHAR, String.class));
        SqlRequest values = Dialect.POSTGRES.buildValues(columns, Arrays.asList(
            new Object[]{"a", PRECISE, true, "quote' comma, slash\\"}, new Object[]{"b", null, null, null})).toRequest();
        return SqlRequests.prepare("insert into values_probe(id,n,b,s) " + values.getSql(), values.getParameters());
    }

    static SqlRequest delete() {
        SqlRequest values = Dialect.POSTGRES.buildValues(List.of(column(JDBCType.VARCHAR, String.class)),
            List.<Object[]>of(new Object[]{"b"})).toRequest();
        return SqlRequests.prepare("delete from values_probe p using (" + values.getSql()
            + ") incoming(id) where p.id=incoming.id", values.getParameters());
    }

    @Test
    public void jdbcBindsTypedArraysAndUsesValuesInInsertAndJoin() throws Exception {
        Assume.assumeTrue(Boolean.getBoolean("easyorm.values.integration"));
        try (Connection connection = DriverManager.getConnection(System.getProperty("easyorm.values.jdbc-url"),
            System.getProperty("easyorm.values.username"), System.getProperty("easyorm.values.password"))) {
            var executor = new TestSyncSqlExecutor(new ConnectionProvider() {
                public Connection getConnection() { return connection; }
                public void releaseConnect(Connection ignored) { }
            });
            executor.execute(SqlRequests.prepare(CREATE));
            assertEquals(2, executor.update(insert()));
            var rows = executor.select("select * from values_probe order by id");
            assertEquals(0, PRECISE.compareTo((BigDecimal) rows.get(0).get("n")));
            assertEquals(true, rows.get(0).get("b"));
            assertEquals("quote' comma, slash\\", rows.get(0).get("s"));
            assertNull(rows.get(1).get("n"));
            assertNull(rows.get(1).get("b"));
            assertEquals(1, executor.update(delete()));
        }
    }

    @Test
    public void r2dbcBindsTypedArraysAndUsesValuesInInsertAndJoin() {
        Assume.assumeTrue(Boolean.getBoolean("easyorm.values.integration"));
        var connection = Mono.from(ConnectionFactories.get(System.getProperty("easyorm.values.r2dbc-url")).create()).block(LIMIT);
        try {
            var executor = new R2dbcReactiveSqlExecutor() {
                protected Mono<io.r2dbc.spi.Connection> getConnection() { return Mono.just(connection); }
                protected void releaseConnection(SignalType type, io.r2dbc.spi.Connection ignored) { }
            };
            executor.execute(SqlRequests.prepare(CREATE)).block(LIMIT);
            assertEquals(Integer.valueOf(2), executor.update(insert()).block(LIMIT));
            var rows = executor.select("select * from values_probe order by id").collectList().block(LIMIT);
            assertEquals(0, PRECISE.compareTo((BigDecimal) rows.get(0).get("n")));
            assertEquals(true, rows.get(0).get("b"));
            assertNull(rows.get(1).get("n"));
            assertNull(rows.get(1).get("b"));
            assertEquals(Integer.valueOf(1), executor.update(delete()).block(LIMIT));
        } finally {
            Mono.from(connection.close()).block(LIMIT);
        }
    }
}
