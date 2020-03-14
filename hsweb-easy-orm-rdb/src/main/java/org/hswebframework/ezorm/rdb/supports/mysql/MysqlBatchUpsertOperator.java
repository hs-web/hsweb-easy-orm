package org.hswebframework.ezorm.rdb.supports.mysql;

import lombok.AllArgsConstructor;
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
import org.hswebframework.ezorm.rdb.operator.dml.upsert.*;
import org.hswebframework.ezorm.rdb.utils.ExceptionUtils;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Set;
import java.util.function.Supplier;

@SuppressWarnings("all")
public class MysqlBatchUpsertOperator implements SaveOrUpdateOperator {

    private RDBTableMetadata table;

    private PostgresqlUpsertBatchInsertSqlBuilder builder;

    private RDBColumnMetadata idColumn;

    private SaveOrUpdateOperator fallback;

    public MysqlBatchUpsertOperator(RDBTableMetadata table) {
        this.table = table;
        this.builder = new PostgresqlUpsertBatchInsertSqlBuilder(table);
        this.idColumn = table.getColumns()
                .stream().filter(RDBColumnMetadata::isPrimaryKey)
                .findFirst().orElse(null);
        this.fallback = new DefaultSaveOrUpdateOperator(table);
    }

    @Override
    public SaveResultOperator execute(UpsertOperatorParameter parameter) {
        if (idColumn == null) {
            this.idColumn = table.getColumns()
                    .stream()
                    .filter(RDBColumnMetadata::isPrimaryKey)
                    .findFirst()
                    .orElse(null);

            if (this.idColumn == null) {
                return fallback.execute(parameter);
            }
        }

        return new PostgresqlSaveResultOperator(() -> builder.build(new MysqlUpsertOperatorParameter(parameter)), parameter.getValues().size());
    }

    class MysqlUpsertOperatorParameter extends InsertOperatorParameter {

        private boolean doNoThingOnConflict;

        public MysqlUpsertOperatorParameter(UpsertOperatorParameter parameter) {
            doNoThingOnConflict = parameter.isDoNothingOnConflict();
            setColumns(parameter.toInsertColumns());
            setValues(parameter.getValues());
        }

    }

    @AllArgsConstructor
    private class PostgresqlSaveResultOperator implements SaveResultOperator {

        Supplier<SqlRequest> sqlRequest;
        int total;

        @Override
        public SaveResult sync() {
            return ExceptionUtils.translation(() -> {
                SyncSqlExecutor sqlExecutor = table.findFeatureNow(SyncSqlExecutor.ID);
                sqlExecutor.update(sqlRequest.get());
                return SaveResult.of(0, total);
            }, table);
        }

        @Override
        public Mono<SaveResult> reactive() {
            return Mono.defer(() -> {
                return Mono.just(sqlRequest.get())
                        .as(table.findFeatureNow(ReactiveSqlExecutor.ID)::update)
                        .map(i -> SaveResult.of(0, total))
                        .as(ExceptionUtils.translation(table));
            });
        }
    }

    private class PostgresqlUpsertBatchInsertSqlBuilder extends BatchInsertSqlBuilder {

        public PostgresqlUpsertBatchInsertSqlBuilder(RDBTableMetadata table) {
            super(table);
        }

        @Override
        protected PrepareSqlFragments beforeBuild(InsertOperatorParameter parameter, PrepareSqlFragments fragments) {
            if (((MysqlUpsertOperatorParameter) parameter).doNoThingOnConflict) {
                return fragments.addSql("insert ignore into")
                        .addSql(table.getFullName());
            }
            return super.beforeBuild(parameter, fragments);
        }

        @Override
        protected PrepareSqlFragments afterBuild(Set<InsertColumn> columns, InsertOperatorParameter parameter, PrepareSqlFragments sql) {

            if (((MysqlUpsertOperatorParameter) parameter).doNoThingOnConflict) {
                return sql;
            }
            sql.addSql("on duplicate key update");

            List<Object> values = parameter.getValues().get(0);

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
                    sql.addSql(",");
                }
                more = true;
                sql.addSql(columnMetadata.getQuoteName()).addSql("=");
                if (value instanceof NativeSql) {
                    sql.addSql(((NativeSql) value).getSql()).addParameter(((NativeSql) value).getParameters());
                    continue;
                }
                sql.addSql("VALUES(", columnMetadata.getQuoteName(), ")");
            }

            return sql;
        }

    }
}
