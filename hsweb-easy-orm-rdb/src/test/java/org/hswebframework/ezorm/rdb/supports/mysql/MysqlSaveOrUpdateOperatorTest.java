package org.hswebframework.ezorm.rdb.supports.mysql;

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
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
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

public class MysqlSaveOrUpdateOperatorTest {

    @Test
    public void testSyncBuildsPerRowUpsertAndClassifiesSaveResult() {
        CapturingSyncSqlExecutor executor = new CapturingSyncSqlExecutor(0, 1);
        RDBTableMetadata table = newTable(executor, null);
        UpsertOperatorParameter parameter = parameter();

        SaveResult result = new MysqlSaveOrUpdateOperator(table).execute(parameter).sync();

        Assert.assertEquals(1, result.getAdded());
        Assert.assertEquals(1, result.getUpdated());
        Assert.assertEquals(2, executor.requests.size());
        Assert.assertTrue(executor.requests.get(0).getSql().contains("on duplicate key update"));
        Assert.assertTrue(executor.requests.get(0).getSql().contains("`name` = concat(?,?)"));
        Assert.assertTrue(executor.requests.get(0).getSql().contains("`description` = ?"));
        Assert.assertTrue(Arrays.asList(executor.requests.get(0).getParameters()).contains("left"));
        Assert.assertTrue(Arrays.asList(executor.requests.get(0).getParameters()).contains("right"));
    }

    @Test
    public void testReactiveBuildsPerRowUpsertAndClassifiesSaveResult() {
        CapturingReactiveSqlExecutor reactive = new CapturingReactiveSqlExecutor(1, 0);
        RDBTableMetadata table = newTable(new CapturingSyncSqlExecutor(1), reactive);
        SaveResult result = new MysqlSaveOrUpdateOperator(table).execute(parameter()).reactive().block();

        Assert.assertEquals(1, result.getAdded());
        Assert.assertEquals(1, result.getUpdated());
        Assert.assertEquals(2, reactive.requests.size());
    }

    private static UpsertOperatorParameter parameter() {
        UpsertOperatorParameter parameter = new UpsertOperatorParameter();
        parameter.getColumns().add(UpsertColumn.of("id", false));
        parameter.getColumns().add(UpsertColumn.of("name", false));
        parameter.getColumns().add(UpsertColumn.of("description", false));
        parameter.getColumns().add(UpsertColumn.of("readonly", false));
        parameter.getColumns().add(UpsertColumn.of("blocked", false));
        parameter.getColumns().add(UpsertColumn.of("ignored", true));
        parameter.getColumns().add(UpsertColumn.of("ghost", false));
        parameter.getValues().add(Arrays.asList("1", NativeSql.of("concat(?,?)", "left", "right"), "desc", "ro", "blocked", "ignored", "ghost"));
        parameter.getValues().add(Arrays.asList("2", null));
        return parameter;
    }

    private static RDBTableMetadata newTable(CapturingSyncSqlExecutor sync, CapturingReactiveSqlExecutor reactive) {
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.MYSQL);
        MysqlSchemaMetadata schema = new MysqlSchemaMetadata("test");
        database.addFeature(sync);
        if (reactive != null) {
            database.addFeature(reactive);
        }
        database.addSchema(schema);
        database.setCurrentSchema(schema);
        RDBTableMetadata table = schema.newTable("save_or_update_test");
        table.addColumn(column("id", true, true, true));
        table.addColumn(column("name", false, true, true));
        table.addColumn(column("description", false, true, true));
        table.addColumn(column("readonly", false, false, true));
        table.addColumn(column("blocked", false, true, false));
        table.addColumn(column("ignored", false, true, true));
        return table;
    }

    private static RDBColumnMetadata column(String name, boolean primary, boolean updatable, boolean saveable) {
        RDBColumnMetadata column = new RDBColumnMetadata();
        column.setName(name);
        column.setAlias(name);
        column.setPrimaryKey(primary);
        column.setUpdatable(updatable);
        column.setSaveable(saveable);
        column.setLength(64);
        column.setType(JdbcDataType.of(JDBCType.VARCHAR, String.class));
        return column;
    }

    private static class CapturingSyncSqlExecutor implements SyncSqlExecutor {
        private final int[] results;
        private int index;
        private final List<SqlRequest> requests = new ArrayList<>();

        private CapturingSyncSqlExecutor(int... results) {
            this.results = results;
        }

        @Override
        public int update(SqlRequest request) {
            requests.add(request);
            return results[Math.min(index++, results.length - 1)];
        }

        @Override
        public void execute(SqlRequest request) {
            requests.add(request);
        }

        @Override
        public <T, R> R select(SqlRequest request, ResultWrapper<T, R> wrapper) {
            throw new UnsupportedOperationException();
        }
    }

    private static class CapturingReactiveSqlExecutor implements ReactiveSqlExecutor {
        private final int[] results;
        private int index;
        private final List<SqlRequest> requests = new ArrayList<>();

        private CapturingReactiveSqlExecutor(int... results) {
            this.results = results;
        }

        @Override
        public Mono<Integer> update(Publisher<SqlRequest> request) {
            return Flux.from(request).map(sql -> {
                requests.add(sql);
                return results[Math.min(index++, results.length - 1)];
            }).next();
        }

        @Override
        public Mono<Void> execute(Publisher<SqlRequest> request) {
            return Flux.from(request).doOnNext(requests::add).then();
        }

        @Override
        public <T> Flux<T> select(Publisher<SqlRequest> request, ResultWrapper<T, ?> wrapper) {
            throw new UnsupportedOperationException();
        }
    }
}
