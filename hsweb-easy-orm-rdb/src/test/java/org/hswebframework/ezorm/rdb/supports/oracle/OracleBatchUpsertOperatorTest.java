package org.hswebframework.ezorm.rdb.supports.oracle;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.defaults.SaveResult;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.SaveOrUpdateOperator;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.UpsertColumn;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.UpsertOperatorParameter;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class OracleBatchUpsertOperatorTest {

    @Test
    public void testUpsertWithoutPrimaryKeyColumnFallbackToBatchInsert() {
        CapturingSyncSqlExecutor sqlExecutor = new CapturingSyncSqlExecutor();
        RDBTableMetadata table = newTable(sqlExecutor);
        table.addColumn(column("id", true));
        table.addColumn(column("name", false));

        UpsertOperatorParameter parameter = new UpsertOperatorParameter();
        parameter.getColumns().add(UpsertColumn.of("name", false));
        parameter.getValues().add(Arrays.asList("test1"));
        parameter.getValues().add(Arrays.asList("test2"));

        SaveResult result = table
            .findFeatureNow(SaveOrUpdateOperator.ID)
            .execute(parameter)
            .sync();

        Assert.assertEquals(2, result.getAdded());
        Assert.assertEquals(0, result.getUpdated());
        Assert.assertNotNull(sqlExecutor.sqlRequest);
        Assert.assertTrue(sqlExecutor.sqlRequest.getSql().toLowerCase().startsWith("insert all"));
        Assert.assertFalse(sqlExecutor.sqlRequest.getSql().toLowerCase().contains("merge into"));
        Assert.assertFalse(sqlExecutor.sqlRequest.getSql().contains("\"ID\""));
        Assert.assertEquals(2, sqlExecutor.sqlRequest.getParameters().length);
    }

    @Test
    public void testDoNothingOnConflictWithoutUpdateColumns() {
        CapturingSyncSqlExecutor sqlExecutor = new CapturingSyncSqlExecutor();
        RDBTableMetadata table = newTable(sqlExecutor);
        table.addColumn(column("id", true));
        table.addColumn(column("name", false));

        UpsertOperatorParameter parameter = new UpsertOperatorParameter();
        parameter.setDoNothingOnConflict(true);
        parameter.getColumns().add(UpsertColumn.of("id", false));
        parameter.getColumns().add(UpsertColumn.of("name", true));
        parameter.getValues().add(Arrays.asList("1", "test"));

        table
            .findFeatureNow(SaveOrUpdateOperator.ID)
            .execute(parameter)
            .sync();

        Assert.assertNotNull(sqlExecutor.sqlRequest);
        Assert.assertTrue(sqlExecutor.sqlRequest.getSql().toLowerCase().contains("merge into"));
        Assert.assertFalse(sqlExecutor.sqlRequest.getSql().toLowerCase().contains("when matched then update set"));
    }

    @Test
    public void testMixedPrimaryKeyValuesSplitToInsertAndMerge() {
        CapturingSyncSqlExecutor sqlExecutor = new CapturingSyncSqlExecutor();
        RDBTableMetadata table = newTable(sqlExecutor);
        table.addColumn(column("id", true));
        table.addColumn(column("name", false));

        UpsertOperatorParameter parameter = new UpsertOperatorParameter();
        parameter.getColumns().add(UpsertColumn.of("id", false));
        parameter.getColumns().add(UpsertColumn.of("name", false));
        parameter.getValues().add(Arrays.asList("1", "with-id"));
        parameter.getValues().add(Arrays.asList(null, "without-id"));

        SaveResult result = table
            .findFeatureNow(SaveOrUpdateOperator.ID)
            .execute(parameter)
            .sync();

        Assert.assertEquals(1, result.getAdded());
        Assert.assertEquals(2, result.getUpdated());
        Assert.assertEquals(2, sqlExecutor.sqlRequests.size());
        Assert.assertTrue(sqlExecutor.sqlRequests.get(0).getSql().toLowerCase().startsWith("insert"));
        Assert.assertFalse(sqlExecutor.sqlRequests.get(0).getSql().contains("\"ID\""));
        Assert.assertTrue(sqlExecutor.sqlRequests.get(1).getSql().toLowerCase().contains("merge into"));
    }

    private static RDBTableMetadata newTable(CapturingSyncSqlExecutor sqlExecutor) {
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.ORACLE);
        OracleSchemaMetadata schema = new OracleSchemaMetadata("PUBLIC");

        database.addFeature(sqlExecutor);
        database.addSchema(schema);
        database.setCurrentSchema(schema);

        return schema.newTable("upsert_test");
    }

    private static RDBColumnMetadata column(String name, boolean primaryKey) {
        RDBColumnMetadata column = new RDBColumnMetadata();
        column.setName(name);
        column.setAlias(name);
        column.setPrimaryKey(primaryKey);
        column.setLength(32);
        column.setType(JdbcDataType.of(JDBCType.VARCHAR, String.class));
        return column;
    }

    private static class CapturingSyncSqlExecutor implements SyncSqlExecutor {

        private SqlRequest sqlRequest;

        private final List<SqlRequest> sqlRequests = new ArrayList<>();

        @Override
        public int update(SqlRequest request) {
            this.sqlRequest = request;
            this.sqlRequests.add(request);
            return request.getParameters().length;
        }

        @Override
        public void execute(SqlRequest request) {
            this.sqlRequest = request;
        }

        @Override
        public <T, R> R select(SqlRequest request, ResultWrapper<T, R> wrapper) {
            throw new UnsupportedOperationException();
        }
    }
}
