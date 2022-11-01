package org.hswebframework.ezorm.rdb.supports.mysql;

import lombok.AllArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.defaults.SaveResult;
import org.hswebframework.ezorm.rdb.metadata.ConstraintType;
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
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Set;
import java.util.function.Supplier;
import java.util.stream.Collectors;

@SuppressWarnings("all")
public class MysqlBatchUpsertOperator implements SaveOrUpdateOperator {

    private RDBTableMetadata table;

    private MysqlUpsertBatchInsertSqlBuilder builder;

    private SaveOrUpdateOperator fallback;

    private Boolean fallbacked;

    public MysqlBatchUpsertOperator(RDBTableMetadata table) {
        this.table = table;
        this.builder = new MysqlUpsertBatchInsertSqlBuilder(table);
        this.fallback = new DefaultSaveOrUpdateOperator(table);
    }

    protected boolean doFallback() {
        if (fallbacked != null) {
            return fallbacked;
        }
        RDBColumnMetadata idColumn = table
                .getColumns()
                .stream()
                .filter(RDBColumnMetadata::isPrimaryKey)
                .findFirst()
                .orElse(null);
        //没有ID
        if (idColumn == null) {
            return fallbacked = true;
        }
        //有指定唯一索引
        if (table
                .getIndexes()
                .stream()
                .anyMatch(index -> index.isUnique() && !index.isPrimaryKey())) {
            return fallbacked = true;
        }
        //有指定唯一约束
        if (CollectionUtils.isNotEmpty(table.getConstraints())) {
            return fallbacked = true;
        }
        return fallbacked = false;
    }

    @Override
    public SaveResultOperator execute(UpsertOperatorParameter parameter) {
        if (doFallback()) {
            return fallback.execute(parameter);
        }
        return new MysqlSaveResultOperator(() -> builder
                .build(new MysqlUpsertOperatorParameter(parameter)), parameter.getValues().size());
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
    private class MysqlSaveResultOperator implements SaveResultOperator {

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

    private class MysqlUpsertBatchInsertSqlBuilder extends BatchInsertSqlBuilder {

        public MysqlUpsertBatchInsertSqlBuilder(RDBTableMetadata table) {
            super(table);
        }

        @Override
        protected PrepareSqlFragments beforeBuild(InsertOperatorParameter parameter, PrepareSqlFragments fragments) {

            ((MysqlUpsertOperatorParameter) parameter).doNoThingOnConflict |= isDoNoThing(parameter.getColumns());

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


            int index = 0;
            boolean more = false;
            for (InsertColumn column : columns) {

                index++;
                if (column instanceof UpsertColumn && ((UpsertColumn) column).isUpdateIgnore()) {
                    continue;
                }
                RDBColumnMetadata columnMetadata = table.getColumn(column.getColumn()).orElse(null);
                if (columnMetadata == null
                        || columnMetadata.isPrimaryKey()
                        || !columnMetadata.isUpdatable()
                        || !columnMetadata.isSaveable()) {

                    continue;
                }
                if (more) {
                    sql.addSql(",");
                }
                more = true;
                sql.addSql(columnMetadata.getQuoteName()).addSql("=");
                sql.addSql(
                        "coalesce(", "VALUES(", columnMetadata.getQuoteName(), ")", ",", columnMetadata.getFullName(), ")"
                );
            }

            return sql;
        }

        private boolean isDoNoThing(Set<InsertColumn> columns) {
            for (InsertColumn column : columns) {

                if (column instanceof UpsertColumn && ((UpsertColumn) column).isUpdateIgnore()) {
                    continue;
                }
                RDBColumnMetadata columnMetadata = table.getColumn(column.getColumn()).orElse(null);
                if (columnMetadata == null
                        || columnMetadata.isPrimaryKey()
                        || !columnMetadata.isUpdatable()
                        || !columnMetadata.isSaveable()) {

                    continue;
                }
                return true;
            }

            return false;
        }

    }
}
