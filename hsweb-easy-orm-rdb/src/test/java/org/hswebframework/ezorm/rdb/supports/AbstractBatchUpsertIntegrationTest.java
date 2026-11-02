package org.hswebframework.ezorm.rdb.supports;

import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.lowerCase;

public abstract class AbstractBatchUpsertIntegrationTest {

    protected abstract RDBSchemaMetadata getSchema();

    protected abstract Dialect getDialect();

    protected abstract SyncSqlExecutor getSqlExecutor();

    @Test
    public void testUpsertWithoutPrimaryKeyColumnUsesRuntimeDefaultOnRealDatabase() {
        IntegrationContext context = newContext();
        AtomicInteger idSequence = new AtomicInteger();

        try {
            createTable(context, () -> "generated-" + idSequence.incrementAndGet());

            context.operator
                .dml()
                .upsert(context.tableName)
                .columns("name")
                .values("test1")
                .values("test2")
                .execute()
                .sync();

            Map<String, Map<String, Object>> rows = rowsByName(context);
            Assert.assertEquals(2, rows.size());
            Assert.assertEquals("generated-1", rows.get("test1").get("id"));
            Assert.assertEquals("generated-2", rows.get("test2").get("id"));
        } finally {
            dropTable(context);
        }
    }

    @Test
    public void testNullPrimaryKeyValuesUseRuntimeDefaultOnRealDatabase() {
        IntegrationContext context = newContext();
        AtomicInteger idSequence = new AtomicInteger();

        try {
            createTable(context, () -> "generated-" + idSequence.incrementAndGet());
            RDBTableMetadata table = context.database
                .getCurrentSchema()
                .getTable(context.tableName, false)
                .orElseThrow(NullPointerException::new);

            context.operator
                .dml()
                .upsert(context.tableName)
                .columns("id", "name")
                .values(null, "null-id")
                .values(NullValue.of(JdbcDataType.of(JDBCType.VARCHAR, String.class)), "typed-null-id")
                .execute()
                .sync();

            Map<String, Map<String, Object>> rows = rowsByName(context);
            Assert.assertEquals(2, rows.size());
            Assert.assertEquals("generated-1", rows.get("null-id").get("id"));
            Assert.assertEquals("generated-2", rows.get("typed-null-id").get("id"));
            Assert.assertTrue(table.getColumn("id").isPresent());
        } finally {
            dropTable(context);
        }
    }

    @Test
    public void testSingleTypedNullPrimaryKeyUsesRuntimeDefaultOnRealDatabase() {
        IntegrationContext context = newContext();
        AtomicInteger idSequence = new AtomicInteger();

        try {
            createTable(context, () -> "generated-" + idSequence.incrementAndGet());

            context.operator
                .dml()
                .upsert(context.tableName)
                .columns("id", "name")
                .values(NullValue.of(JdbcDataType.of(JDBCType.VARCHAR, String.class)), "single-typed-null-id")
                .execute()
                .sync();

            Map<String, Map<String, Object>> rows = rowsByName(context);
            Assert.assertEquals(1, rows.size());
            Assert.assertEquals("generated-1", rows.get("single-typed-null-id").get("id"));
        } finally {
            dropTable(context);
        }
    }

    @Test
    public void testMixedPrimaryKeyValuesInsertAndUpdateOnRealDatabase() {
        IntegrationContext context = newContext();
        AtomicInteger idSequence = new AtomicInteger();

        try {
            createTable(context, () -> "generated-" + idSequence.incrementAndGet());

            context.operator
                .dml()
                .upsert(context.tableName)
                .columns("id", "name")
                .values("fixed-id", "first")
                .execute()
                .sync();

            context.operator
                .dml()
                .upsert(context.tableName)
                .columns("id", "name")
                .values("fixed-id", "updated")
                .values(null, "generated")
                .execute()
                .sync();

            Map<String, Map<String, Object>> rows = rowsByName(context);
            Assert.assertEquals(2, rows.size());
            Assert.assertEquals("fixed-id", rows.get("updated").get("id"));
            Assert.assertEquals("generated-1", rows.get("generated").get("id"));
        } finally {
            dropTable(context);
        }
    }

    private IntegrationContext newContext() {
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(getDialect());
        RDBSchemaMetadata schema = getSchema();
        database.addFeature(getSqlExecutor());
        database.addSchema(schema);
        database.setCurrentSchema(schema);

        IntegrationContext context = new IntegrationContext();
        context.database = database;
        context.operator = DefaultDatabaseOperator.of(database);
        context.tableName = "UP_IT_" + Long.toString(System.nanoTime(), 36).toUpperCase();
        return context;
    }

    private void createTable(IntegrationContext context, RuntimeDefaultValue idGenerator) {
        context.operator
            .ddl()
            .createOrAlter(context.tableName)
            .addColumn("id")
            .defaultValueRuntime(idGenerator)
            .primaryKey()
            .varchar(32)
            .commit()
            .addColumn("name")
            .varchar(64)
            .commit()
            .commit()
            .sync();
    }

    private Map<String, Map<String, Object>> rowsByName(IntegrationContext context) {
        List<Map<String, Object>> rows = context.operator
            .dml()
            .query(context.tableName)
            .select("id", "name")
            .fetch(lowerCase(ResultWrappers.mapList()))
            .sync();

        return rows
            .stream()
            .collect(Collectors.toMap(
                row -> String.valueOf(row.get("name")),
                row -> new HashMap<>(row)
            ));
    }

    private void dropTable(IntegrationContext context) {
        try {
            RDBTableMetadata table = context.database
                .getCurrentSchema()
                .getTable(context.tableName, false)
                .orElse(null);
            String tableName = table == null ? context.tableName : table.getFullName();
            context.operator
                .sql()
                .sync()
                .execute(SqlRequests.of("drop table " + tableName));
        } catch (Throwable ignore) {
        }
    }

    private static class IntegrationContext {
        private RDBDatabaseMetadata database;
        private DatabaseOperator operator;
        private String tableName;
    }
}
