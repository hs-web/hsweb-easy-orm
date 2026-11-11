package org.hswebframework.ezorm.rdb.supports.json;

import org.hswebframework.ezorm.rdb.executor.BatchSqlRequest;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.MetadataHelper;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertColumn;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperatorParameter;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.SaveOrUpdateOperator;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.UpsertColumn;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.UpsertOperatorParameter;
import org.hswebframework.ezorm.rdb.supports.mssql.SqlServerBatchUpsertOperator;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlBatchUpsertOperator;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlSaveOrUpdateOperator;
import org.hswebframework.ezorm.rdb.supports.oracle.OracleBatchUpsertOperator;
import org.hswebframework.ezorm.rdb.supports.oracle.OracleInsertSqlBuilder;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlBatchUpsertOperator;
import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.*;

/** 验证方言实际生成的 SQL 参数，SQL 表达式不能泄漏到驱动绑定参数。 */
public class JsonNullWriteCompatibilityTest {

    private RDBTableMetadata table(JsonType type) {
        RDBTableMetadata table = MetadataHelper.createMockSchema().getTable("test").orElseThrow(AssertionError::new);
        table.getColumnNow("id").setPrimaryKey(true);
        table.getColumnNow("name").setType(type);
        table.getColumnNow("name").setValueCodec(type);
        return table;
    }

    private static class Capture implements SyncSqlExecutor {
        private SqlRequest request;
        @Override public int update(SqlRequest request) { this.request = request; return 1; }
        @Override public void execute(SqlRequest request) { throw new AssertionError("unexpected execute"); }
        @Override public <T, R> R select(SqlRequest request, ResultWrapper<T, R> wrapper) {
            throw new AssertionError("unexpected select");
        }
    }

    private void assertBindable(SqlRequest request) {
        assertNotNull("SQL request was captured", request);
        for (Object value : request.getParameters()) {
            assertFalse("SQL expression leaked into bind parameters: " + request.getSql(), value instanceof NativeSql);
        }
        if (request instanceof BatchSqlRequest) {
            for (SqlRequest row : ((BatchSqlRequest) request).getBatch()) {
                assertBindable(row);
            }
        }
    }

    private void verifyUpsert(RDBTableMetadata table, SaveOrUpdateOperator operator, Object value) {
        Capture capture = new Capture();
        table.addFeature(capture);
        UpsertOperatorParameter parameter = new UpsertOperatorParameter();
        for (String name : Arrays.asList("id", "name")) {
            UpsertColumn column = new UpsertColumn();
            column.setColumn(name);
            parameter.getColumns().add(column);
        }
        parameter.getValues().add(Arrays.asList("one", value));
        operator.execute(parameter).sync();
        assertBindable(capture.request);
    }

    @Test
    public void oracleClobInsertKeepsNullBindable() {
        RDBTableMetadata table = table(JsonType.CLOB);
        InsertOperatorParameter parameter = new InsertOperatorParameter();
        for (String name : Arrays.asList("id", "name")) {
            InsertColumn column = new InsertColumn();
            column.setColumn(name);
            parameter.getColumns().add(column);
        }
        parameter.getValues().add(Arrays.asList("one", null));
        assertBindable(OracleInsertSqlBuilder.of(table).build(parameter));
    }

    @Test
    public void oracleClobMergeKeepsExplicitNullBindable() {
        RDBTableMetadata table = table(JsonType.CLOB);
        verifyUpsert(table, new OracleBatchUpsertOperator(table), NullValue.of(JsonType.CLOB));
    }

    @Test
    public void sqlServerMergeKeepsExplicitNullBindable() {
        RDBTableMetadata table = table(JsonType.NVARCHAR_MAX);
        verifyUpsert(table, new SqlServerBatchUpsertOperator(table), NullValue.of(JsonType.NVARCHAR_MAX));
    }

    @Test
    public void legacyMysqlUpsertKeepsExplicitNullBindable() {
        RDBTableMetadata table = table(JsonType.INSTANCE);
        verifyUpsert(table, new MysqlSaveOrUpdateOperator(table), NullValue.of(JsonType.INSTANCE));
    }

    @Test
    public void legacyMysqlUpsertKeepsNonNullBindable() {
        RDBTableMetadata table = table(JsonType.INSTANCE);
        verifyUpsert(table, new MysqlSaveOrUpdateOperator(table), "{\"ok\":true}");
    }

    @Test
    public void defaultMysqlUpsertKeepsExplicitNullBindable() {
        RDBTableMetadata table = table(JsonType.INSTANCE);
        verifyUpsert(table, new MysqlBatchUpsertOperator(table), NullValue.of(JsonType.INSTANCE));
    }

    @Test
    public void defaultPostgresUpsertKeepsExpressionsOutOfParameters() {
        for (JsonType type : Arrays.asList(org.hswebframework.ezorm.rdb.supports.postgres.JsonType.INSTANCE,
                                          org.hswebframework.ezorm.rdb.supports.postgres.JsonbType.INSTANCE)) {
            RDBTableMetadata table = table(type);
            verifyUpsert(table, new PostgresqlBatchUpsertOperator(table), NullValue.of(type));
            verifyUpsert(table, new PostgresqlBatchUpsertOperator(table), "{\"ok\":true}");
        }
    }
}
