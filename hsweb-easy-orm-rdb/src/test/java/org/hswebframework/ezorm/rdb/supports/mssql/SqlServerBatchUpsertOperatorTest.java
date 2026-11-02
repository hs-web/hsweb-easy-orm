package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.core.DefaultValue;
import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
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
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.sql.JDBCType;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

public class SqlServerBatchUpsertOperatorTest {

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
        Assert.assertTrue(sqlExecutor.sqlRequest.getSql().toLowerCase().startsWith("insert into"));
        Assert.assertFalse(sqlExecutor.sqlRequest.getSql().toLowerCase().contains("merge into"));
        Assert.assertFalse(sqlExecutor.sqlRequest.getSql().contains("[id]"));
        Assert.assertEquals(2, sqlExecutor.sqlRequest.getParameters().length);
    }

    @Test
    public void testUpsertWithoutPrimaryKeyColumnUsesRuntimeDefaultWhenFallbackToInsert() {
        CapturingSyncSqlExecutor sqlExecutor = new CapturingSyncSqlExecutor();
        RDBTableMetadata table = newTable(sqlExecutor);
        RDBColumnMetadata id = column("id", true);
        id.setDefaultValue(DefaultValue.runtime("generated-id"));
        table.addColumn(id);
        table.addColumn(column("name", false));

        UpsertOperatorParameter parameter = new UpsertOperatorParameter();
        parameter.getColumns().add(UpsertColumn.of("name", false));
        parameter.getValues().add(Arrays.asList("test"));

        table
            .findFeatureNow(SaveOrUpdateOperator.ID)
            .execute(parameter)
            .sync();

        Assert.assertNotNull(sqlExecutor.sqlRequest);
        Assert.assertTrue(sqlExecutor.sqlRequest.getSql().contains("[id]"));
        Assert.assertTrue(Arrays
                              .asList(sqlExecutor.sqlRequest.getParameters())
                              .contains("generated-id"));
    }

    @Test
    public void testNullPrimaryKeyValueUsesRuntimeDefaultWhenFallbackToInsert() {
        CapturingSyncSqlExecutor sqlExecutor = new CapturingSyncSqlExecutor();
        RDBTableMetadata table = newTable(sqlExecutor);
        RDBColumnMetadata id = column("id", true);
        id.setDefaultValue(DefaultValue.runtime("generated-id"));
        table.addColumn(id);
        table.addColumn(column("name", false));

        UpsertOperatorParameter parameter = new UpsertOperatorParameter();
        parameter.getColumns().add(UpsertColumn.of("id", false));
        parameter.getColumns().add(UpsertColumn.of("name", false));
        parameter.getValues().add(Arrays.asList(null, "without-id"));

        table
            .findFeatureNow(SaveOrUpdateOperator.ID)
            .execute(parameter)
            .sync();

        Assert.assertNotNull(sqlExecutor.sqlRequest);
        Assert.assertTrue(sqlExecutor.sqlRequest.getSql().contains("[id]"));
        Assert.assertTrue(Arrays
                              .asList(sqlExecutor.sqlRequest.getParameters())
                              .contains("generated-id"));
    }

    @Test
    public void testNullValuePrimaryKeyUsesRuntimeDefaultWhenFallbackToInsert() {
        CapturingSyncSqlExecutor sqlExecutor = new CapturingSyncSqlExecutor();
        RDBTableMetadata table = newTable(sqlExecutor);
        RDBColumnMetadata id = column("id", true);
        id.setDefaultValue(DefaultValue.runtime("generated-id"));
        table.addColumn(id);
        table.addColumn(column("name", false));

        UpsertOperatorParameter parameter = new UpsertOperatorParameter();
        parameter.getColumns().add(UpsertColumn.of("id", false));
        parameter.getColumns().add(UpsertColumn.of("name", false));
        parameter.getValues().add(Arrays.asList(NullValue.of(id.getType()), "without-id"));

        table
            .findFeatureNow(SaveOrUpdateOperator.ID)
            .execute(parameter)
            .sync();

        Assert.assertNotNull(sqlExecutor.sqlRequest);
        Assert.assertTrue(sqlExecutor.sqlRequest.getSql().contains("[id]"));
        Assert.assertTrue(Arrays
                              .asList(sqlExecutor.sqlRequest.getParameters())
                              .contains("generated-id"));
    }

    @Test
    public void testBatchInsertWithoutPrimaryKeyColumnGeneratesRuntimeDefaultForEachRow() {
        CapturingSyncSqlExecutor sqlExecutor = new CapturingSyncSqlExecutor();
        RDBTableMetadata table = newTable(sqlExecutor);
        RDBColumnMetadata id = column("id", true);
        AtomicInteger idSequence = new AtomicInteger();
        id.setDefaultValue((RuntimeDefaultValue) () -> "generated-" + idSequence.incrementAndGet());
        table.addColumn(id);
        table.addColumn(column("name", false));

        UpsertOperatorParameter parameter = new UpsertOperatorParameter();
        parameter.getColumns().add(UpsertColumn.of("name", false));
        parameter.getValues().add(Arrays.asList("test1"));
        parameter.getValues().add(Arrays.asList("test2"));

        table
            .findFeatureNow(SaveOrUpdateOperator.ID)
            .execute(parameter)
            .sync();

        List<Object> parameters = Arrays.asList(sqlExecutor.sqlRequest.getParameters());
        Assert.assertTrue(sqlExecutor.sqlRequest.getSql().contains("[id]"));
        Assert.assertEquals(4, parameters.size());
        Assert.assertTrue(parameters.contains("generated-1"));
        Assert.assertTrue(parameters.contains("generated-2"));
    }

    @Test
    public void testMixedPrimaryKeyValuesWithRuntimeDefaultSplitToInsertAndMerge() {
        CapturingSyncSqlExecutor sqlExecutor = new CapturingSyncSqlExecutor();
        RDBTableMetadata table = newTable(sqlExecutor);
        RDBColumnMetadata id = column("id", true);
        id.setDefaultValue(DefaultValue.runtime("generated-id"));
        table.addColumn(id);
        table.addColumn(column("name", false));

        UpsertOperatorParameter parameter = new UpsertOperatorParameter();
        parameter.getColumns().add(UpsertColumn.of("id", false));
        parameter.getColumns().add(UpsertColumn.of("name", false));
        parameter.getValues().add(Arrays.asList("1", "with-id"));
        parameter.getValues().add(Arrays.asList(null, "without-id"));

        table
            .findFeatureNow(SaveOrUpdateOperator.ID)
            .execute(parameter)
            .sync();

        Assert.assertEquals(2, sqlExecutor.sqlRequests.size());
        Assert.assertTrue(sqlExecutor.sqlRequests.get(0).getSql().toLowerCase().startsWith("insert into"));
        Assert.assertTrue(sqlExecutor.sqlRequests.get(0).getSql().contains("[id]"));
        Assert.assertTrue(Arrays
                              .asList(sqlExecutor.sqlRequests.get(0).getParameters())
                              .contains("generated-id"));
        Assert.assertTrue(sqlExecutor.sqlRequests.get(1).getSql().toLowerCase().contains("merge into"));
    }

    @Test
    public void testReactiveMixedPrimaryKeyValuesWithRuntimeDefaultSplitToInsertAndMerge() {
        CapturingReactiveSqlExecutor sqlExecutor = new CapturingReactiveSqlExecutor();
        RDBTableMetadata table = newTable(new CapturingSyncSqlExecutor(), sqlExecutor);
        RDBColumnMetadata id = column("id", true);
        id.setDefaultValue(DefaultValue.runtime("generated-id"));
        table.addColumn(id);
        table.addColumn(column("name", false));

        UpsertOperatorParameter parameter = new UpsertOperatorParameter();
        parameter.getColumns().add(UpsertColumn.of("id", false));
        parameter.getColumns().add(UpsertColumn.of("name", false));
        parameter.getValues().add(Arrays.asList("1", "with-id"));
        parameter.getValues().add(Arrays.asList(null, "without-id"));

        table
            .findFeatureNow(SaveOrUpdateOperator.ID)
            .execute(parameter)
            .reactive()
            .block();

        Assert.assertEquals(2, sqlExecutor.sqlRequests.size());
        Assert.assertTrue(sqlExecutor.sqlRequests.get(0).getSql().toLowerCase().startsWith("insert into"));
        Assert.assertTrue(sqlExecutor.sqlRequests.get(0).getSql().contains("[id]"));
        Assert.assertTrue(Arrays
                              .asList(sqlExecutor.sqlRequests.get(0).getParameters())
                              .contains("generated-id"));
        Assert.assertTrue(sqlExecutor.sqlRequests.get(1).getSql().toLowerCase().contains("merge into"));
    }

    @Test
    public void testPrimaryKeyColumnMatchIgnoreCaseByAlias() {
        CapturingSyncSqlExecutor sqlExecutor = new CapturingSyncSqlExecutor();
        RDBTableMetadata table = newTable(sqlExecutor);
        table.addColumn(column("ID", "id", true));
        table.addColumn(column("name", false));

        UpsertOperatorParameter parameter = new UpsertOperatorParameter();
        parameter.getColumns().add(UpsertColumn.of("id", false));
        parameter.getColumns().add(UpsertColumn.of("name", false));
        parameter.getValues().add(Arrays.asList("1", "test"));

        table
            .findFeatureNow(SaveOrUpdateOperator.ID)
            .execute(parameter)
            .sync();

        Assert.assertNotNull(sqlExecutor.sqlRequest);
        Assert.assertTrue(sqlExecutor.sqlRequest.getSql().toLowerCase().contains("merge into"));
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
        Assert.assertTrue(sqlExecutor.sqlRequests.get(0).getSql().toLowerCase().startsWith("insert into"));
        Assert.assertFalse(sqlExecutor.sqlRequests.get(0).getSql().contains("[id]"));
        Assert.assertTrue(sqlExecutor.sqlRequests.get(1).getSql().toLowerCase().contains("merge into"));
    }

    private static RDBTableMetadata newTable(CapturingSyncSqlExecutor sqlExecutor) {
        return newTable(sqlExecutor, null);
    }

    private static RDBTableMetadata newTable(
        CapturingSyncSqlExecutor sqlExecutor,
        CapturingReactiveSqlExecutor reactiveSqlExecutor) {
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.MSSQL);
        SqlServerSchemaMetadata schema = new SqlServerSchemaMetadata("dbo");

        database.addFeature(sqlExecutor);
        if (reactiveSqlExecutor != null) {
            database.addFeature(reactiveSqlExecutor);
        }
        database.addSchema(schema);
        database.setCurrentSchema(schema);

        return schema.newTable("upsert_test");
    }

    private static RDBColumnMetadata column(String name, boolean primaryKey) {
        return column(name, name, primaryKey);
    }

    private static RDBColumnMetadata column(String name, String alias, boolean primaryKey) {
        RDBColumnMetadata column = new RDBColumnMetadata();
        column.setName(name);
        column.setAlias(alias);
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

    private static class CapturingReactiveSqlExecutor implements ReactiveSqlExecutor {

        private SqlRequest sqlRequest;

        private final List<SqlRequest> sqlRequests = new ArrayList<>();

        @Override
        public Mono<Integer> update(Publisher<SqlRequest> request) {
            return Flux
                .from(request)
                .map(this::captureUpdate)
                .reduce(0, Integer::sum);
        }

        private int captureUpdate(SqlRequest request) {
            this.sqlRequest = request;
            this.sqlRequests.add(request);
            return request.getParameters().length;
        }

        @Override
        public Mono<Void> execute(Publisher<SqlRequest> request) {
            return Flux
                .from(request)
                .doOnNext(sqlRequest -> this.sqlRequest = sqlRequest)
                .then();
        }

        @Override
        public <T> Flux<T> select(Publisher<SqlRequest> request, ResultWrapper<T, ?> wrapper) {
            throw new UnsupportedOperationException();
        }
    }
}
