package org.hswebframework.ezorm.rdb.supports.mysql;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.defaults.SaveResult;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.insert.BatchInsertSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertColumn;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperatorParameter;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.SaveOrUpdateOperator;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.SaveResultOperator;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class MysqlSaveOrUpdateOperator implements SaveOrUpdateOperator {

    private RDBTableMetadata table;

    public MysqlUpsertBatchInsertSqlBuilder builder;

    public MysqlSaveOrUpdateOperator(RDBTableMetadata table) {
        this.table = table;
        this.builder = new MysqlUpsertBatchInsertSqlBuilder(table);
    }

    @Override
    public SaveResultOperator execute(InsertOperatorParameter parameter) {

        return new MysqlSaveResultOperator(parameter.getValues()
                .stream()
                .map(value -> {
                    InsertOperatorParameter newParam = new InsertOperatorParameter();
                    newParam.setColumns(parameter.getColumns());
                    newParam.getValues().add(value);
                    return newParam;
                })
                .map(builder::build)
                .collect(Collectors.toList()));
    }

    @AllArgsConstructor
    private class MysqlSaveResultOperator implements SaveResultOperator {

        List<SqlRequest> sqlRequest;

        @Override
        public SaveResult sync() {
            SyncSqlExecutor sqlExecutor = table.findFeatureNow(SyncSqlExecutor.ID);

            int added = 0, updated = 0;
            for (SqlRequest request : sqlRequest) {
                int num = sqlExecutor.update(request);
                added += num;
                if (num == 0) {
                    updated++;
                }
            }
            return SaveResult.of(added, updated);
        }

        @Override
        public Mono<SaveResult> reactive() {
            ReactiveSqlExecutor sqlExecutor = table.findFeatureNow(ReactiveSqlExecutor.ID);
            return Flux.fromIterable(sqlRequest)
                    .flatMap(sql -> sqlExecutor.update(Mono.just(sql)))
                    .map(i -> SaveResult.of(i > 0 ? i : 0, i == 0 ? 1 : 0))
                    .reduce(SaveResult::merge);
        }
    }

    private class MysqlUpsertBatchInsertSqlBuilder extends BatchInsertSqlBuilder {

        public MysqlUpsertBatchInsertSqlBuilder(RDBTableMetadata table) {
            super(table);
        }

        @Override
        protected void afterValues(Set<InsertColumn> columns, List<Object> values, PrepareSqlFragments sql) {
            sql.addSql("on duplicate key update");

            int index = 0;
            for (InsertColumn column : columns) {
                try {
                    RDBColumnMetadata columnMetadata = table.getColumn(column.getColumn()).orElse(null);
                    if (columnMetadata == null
                            || columnMetadata.isPrimaryKey()
                            || !columnMetadata.isUpdatable()) {

                        continue;
                    }
                    if (index > 0) {
                        sql.addSql(",");
                    }
                    sql.addSql(columnMetadata.getQuoteName())
                            .addSql("=");

                    Object value = index >= values.size() ? null : values.get(index);
                    if (value == null && columnMetadata.isNotNull()) {
                        continue;
                    }
                    if (value instanceof NativeSql) {
                        sql.addSql(((NativeSql) value).getSql()).addParameter(((NativeSql) value).getParameters());
                        continue;
                    }
                    if (value == null) {
                        value = NullValue.of(columnMetadata.getType());
                    }
                    sql.addSql("?").addParameter(columnMetadata.encode(value));
                } finally {
                    index++;
                }

            }
        }
    }
}
