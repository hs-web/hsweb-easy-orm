package org.hswebframework.ezorm.rdb.supports.mysql;

import org.hswebframework.ezorm.rdb.ConnectionProvider;
import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.UpsertOperator;
import org.junit.After;
import org.junit.Assume;
import org.junit.Before;
import org.junit.Test;
import lombok.SneakyThrows;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.JDBCType;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicInteger;

import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.mapList;
import static org.junit.Assert.*;

/** Real MySQL verification against an explicitly configured disposable test database. */
public class MysqlNativeConflictIntegrationTest {

    private CountingExecutor executor;
    private DefaultDatabaseOperator operator;
    private RDBTableMetadata table;

    @Before
    public void setUp() {
        String url = System.getProperty("ezorm.native.mysql.url");
        Assume.assumeNotNull(url);
        String user = System.getProperty("ezorm.native.mysql.user", "root");
        String password = System.getProperty("ezorm.native.mysql.password", "root");
        executor = new CountingExecutor(new ConnectionProvider() {
            @Override
            @SneakyThrows
            public Connection getConnection() {
                return DriverManager.getConnection(url, user, password);
            }

            @Override
            @SneakyThrows
            public void releaseConnect(Connection connection) {
                connection.close();
            }
        });
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.MYSQL);
        MysqlSchemaMetadata schema = new MysqlSchemaMetadata("extension_test");
        database.addSchema(schema);
        database.setCurrentSchema(schema);
        database.addFeature(executor);
        table = schema.newTable("native_conflict_" + UUID.randomUUID().toString().replace("-", "").substring(0, 12));
        table.addColumn(column("id", true));
        table.addColumn(column("code", false));
        table.addColumn(column("value", false));
        RDBIndexMetadata unique = new RDBIndexMetadata("uk_code");
        unique.setUnique(true);
        unique.setColumns(Arrays.asList(RDBIndexMetadata.IndexColumn.of("code", RDBIndexMetadata.IndexSort.asc)));
        table.addIndex(unique);
        schema.addTable(table);
        operator = DefaultDatabaseOperator.of(database);
        executor.execute(SqlRequests.of("create table " + table.getQuoteName()
            + " (`id` varchar(64) primary key, `code` varchar(64) not null unique, `value` varchar(64))"));
    }

    @After
    public void tearDown() {
        if (table != null) {
            executor.execute(SqlRequests.of("drop table " + table.getQuoteName()));
        }
    }

    @Test
    public void mixedInsertAndUpdateUseOneStatementEachBatch() {
        int statements = executor.updates.get();
        batch("old", 0, 100).execute().sync();
        batch("new", 50, 150).execute().sync();
        assertEquals(2, executor.updates.get() - statements);
        assertEquals(150, executor.select(SqlRequests.of("select id from " + table.getQuoteName()), mapList()).size());
        assertEquals(100, executor.select(SqlRequests.of("select id from " + table.getQuoteName()
            + " where `value` = 'new'"), mapList()).size());
    }

    @Test
    public void anyUniqueConflictUpdatesExistingRowWithoutReplacingItsPrimaryKey() {
        operator.dml().upsert(table).nativeConflict(true).columns("id", "code", "value")
                .values("original", "shared", "before").execute().sync();
        operator.dml().upsert(table).nativeConflict(true).columns("id", "code", "value")
                .values("different", "shared", "after").execute().sync();
        List<java.util.Map<String, Object>> rows = executor.select(SqlRequests.of(
            "select id, `value` from " + table.getQuoteName()), mapList());
        assertEquals(1, rows.size());
        assertEquals("original", rows.get(0).get("id"));
        assertEquals("after", rows.get(0).get("value"));
    }

    @Test
    public void concurrentSameIdentityBatchesRemainAtomicAndDoNotDuplicate() throws Exception {
        ExecutorService pool = Executors.newFixedThreadPool(4);
        try {
            List<Callable<Void>> calls = new ArrayList<>();
            for (int writer = 0; writer < 4; writer++) {
                String value = "writer-" + writer;
                calls.add(() -> {
                    for (int i = 0; i < 10; i++) {
                        batch(value, 0, 100).execute().sync();
                    }
                    return null;
                });
            }
            int statements = executor.updates.get();
            for (Future<Void> future : pool.invokeAll(calls)) {
                future.get();
            }
            assertEquals(40, executor.updates.get() - statements);
            assertEquals(100, executor.select(SqlRequests.of("select id from " + table.getQuoteName()), mapList()).size());
        } finally {
            pool.shutdownNow();
        }
    }

    private UpsertOperator batch(String value, int from, int to) {
        UpsertOperator upsert = operator.dml().upsert(table).nativeConflict(true).columns("id", "code", "value");
        for (int i = from; i < to; i++) {
            upsert.values("id-" + i, "code-" + i, value);
        }
        return upsert;
    }

    private static RDBColumnMetadata column(String name, boolean primary) {
        RDBColumnMetadata column = new RDBColumnMetadata();
        column.setName(name);
        column.setAlias(name);
        column.setPrimaryKey(primary);
        column.setLength(64);
        column.setType(JdbcDataType.of(JDBCType.VARCHAR, String.class));
        return column;
    }

    private static final class CountingExecutor extends TestSyncSqlExecutor {
        private final AtomicInteger updates = new AtomicInteger();

        private CountingExecutor(ConnectionProvider provider) {
            super(provider);
        }

        @Override
        public int update(SqlRequest request) {
            updates.incrementAndGet();
            return super.update(request);
        }
    }
}
