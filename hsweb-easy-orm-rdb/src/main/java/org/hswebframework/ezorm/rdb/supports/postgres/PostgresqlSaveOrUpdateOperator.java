package org.hswebframework.ezorm.rdb.supports.postgres;

import lombok.AllArgsConstructor;
import org.apache.commons.collections.CollectionUtils;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.defaults.SaveResult;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.insert.BatchInsertSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertColumn;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperatorParameter;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.*;
import org.hswebframework.ezorm.rdb.utils.ExceptionUtils;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.Supplier;
import java.util.stream.Collectors;

@SuppressWarnings("all")
public class PostgresqlSaveOrUpdateOperator implements SaveOrUpdateOperator {

    private RDBTableMetadata table;

    private PostgresqlUpsertBatchInsertSqlBuilder builder;

    private RDBColumnMetadata idColumn;

    private SaveOrUpdateOperator fallback;

    public PostgresqlSaveOrUpdateOperator(RDBTableMetadata table) {
        this.table = table;
        this.builder = new PostgresqlUpsertBatchInsertSqlBuilder(table);
        this.idColumn = table.getPrimaryMetadata();
        this.fallback = new DefaultSaveOrUpdateOperator(table);
    }

    @Override
    public SaveResultOperator execute(UpsertOperatorParameter parameter) {
        if (idColumn == null) {
            this.idColumn = table.getPrimaryMetadata();

            if (this.idColumn == null) {
                return fallback.execute(parameter);
            }
        }
        return new PostgresqlSaveResultOperator(() -> parameter.getValues()
                                                               .stream()
                                                               .map(value -> {
                                                                   InsertOperatorParameter newParam = new InsertOperatorParameter();
                                                                   newParam.setColumns(parameter.toInsertColumns());
                                                                   newParam.getValues().add(value);
                                                                   return newParam;
                                                               })
                                                               .map(builder::build)
                                                               .collect(Collectors.toList()));
    }

    @AllArgsConstructor
    private class PostgresqlSaveResultOperator implements SaveResultOperator {

        Supplier<List<SqlRequest>> sqlRequest;

        @Override
        public SaveResult sync() {
            return ExceptionUtils.translation(() -> {
                SyncSqlExecutor sqlExecutor = table.findFeatureNow(SyncSqlExecutor.ID);
                int added = 0, updated = 0;
                for (SqlRequest request : sqlRequest.get()) {
                    int num = sqlExecutor.update(request);
                    added += num;
                    if (num == 0) {
                        updated++;
                    }
                }
                return SaveResult.of(added, updated);
            }, table);
        }

        @Override
        public Mono<SaveResult> reactive() {
            return Mono.defer(() -> {
                ReactiveSqlExecutor sqlExecutor = table.findFeatureNow(ReactiveSqlExecutor.ID);
                return Flux.fromIterable(sqlRequest.get())
                           .flatMap(sql -> sqlExecutor.update(Mono.just(sql)))
                           .map(i -> SaveResult.of(i > 0 ? i : 0, i == 0 ? 1 : 0))
                           .reduce(SaveResult::merge);
            });
        }
    }

    private class PostgresqlUpsertBatchInsertSqlBuilder extends BatchInsertSqlBuilder {

        public PostgresqlUpsertBatchInsertSqlBuilder(RDBTableMetadata table) {
            super(table);
        }

        @Override
        protected void afterValues(Set<InsertColumn> columns, List<Object> values, PrepareSqlFragments sql) {

            List<String> sqlItem = new ArrayList<>(64);
            List<Object> parameters = new ArrayList<>(8);
            sqlItem.add("on conflict (" + idColumn.getName() + ") do update set");

            int index = 0;
            boolean more = false;
            for (InsertColumn column : columns) {
                Object value = index >= values.size() ? null : values.get(index);
                index++;
                if (column instanceof UpsertColumn && ((UpsertColumn) column).isUpdateIgnore()) {
                    continue;
                }
                RDBColumnMetadata columnMetadata = table.getColumn(column.getColumn()).orElse(null);
                if (value == null
                        || columnMetadata == null
                        || columnMetadata.isPrimaryKey()
                        || !columnMetadata.isUpdatable()) {

                    continue;
                }
                if (more) {
                    sqlItem.add(",");
                }
                more = true;
                sqlItem.add(columnMetadata.getQuoteName());
                sqlItem.add("=");
                if (value instanceof NativeSql) {
                    sqlItem.add(((NativeSql) value).getSql());
                    parameters.add(((NativeSql) value).getParameters());
                    continue;
                }
                sqlItem.add("?");
                parameters.add(columnMetadata.encode(value));
            }
            if (!more) {
                sqlItem.add("on conflict (" + idColumn.getName() + ") do nothing");
            }
            sql.addSql(sqlItem);
            sql.addParameter(parameters);
        }
    }
}
