package org.hswebframework.ezorm.rdb.supports.postgres;

import lombok.AllArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.defaults.SaveResult;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.*;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.insert.BatchInsertSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertColumn;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperatorParameter;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.*;
import org.hswebframework.ezorm.rdb.utils.ExceptionUtils;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.Supplier;

@SuppressWarnings("all")
public class PostgresqlBatchUpsertOperator implements SaveOrUpdateOperator {

    private RDBTableMetadata table;

    private PostgresqlUpsertBatchInsertSqlBuilder builder;

    private RDBColumnMetadata idColumn;

    private SaveOrUpdateOperator fallback;

    public PostgresqlBatchUpsertOperator(RDBTableMetadata table) {
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
            this.idColumn = table
                    .getColumns()
                    .stream()
                    .filter(RDBColumnMetadata::isPrimaryKey)
                    .findFirst()
                    .orElse(null);

            if (this.idColumn == null) {
                return fallback.execute(parameter);
            }
        }

        return new PostgresqlSaveResultOperator(() -> builder.build(new PostgresqlUpsertOperatorParameter(parameter)));
    }

    class PostgresqlUpsertOperatorParameter extends InsertOperatorParameter {

        private boolean doNoThingOnConflict;

        private List<Term> where;

        public PostgresqlUpsertOperatorParameter(UpsertOperatorParameter parameter) {
            doNoThingOnConflict = parameter.isDoNothingOnConflict();
            setColumns(parameter.toInsertColumns());
            setValues(parameter.getValues());
            where = parameter.getWhere();
        }

    }

    @AllArgsConstructor
    private class PostgresqlSaveResultOperator implements SaveResultOperator {

        Supplier<SqlRequest> sqlRequest;

        @Override
        public SaveResult sync() {
            return ExceptionUtils.translation(() -> {
                SyncSqlExecutor sqlExecutor = table.findFeatureNow(SyncSqlExecutor.ID);
                int updated = sqlExecutor.update(sqlRequest.get());
                return SaveResult.of(0, updated);
            }, table);
        }

        @Override
        public Mono<SaveResult> reactive() {
            return Mono
                    .fromSupplier(sqlRequest)
                    .as(table.findFeatureNow(ReactiveSqlExecutor.ID)::update)
                    .map(i -> SaveResult.of(0, i))
                    .as(ExceptionUtils.translation(table));
        }
    }

    private class PostgresqlUpsertBatchInsertSqlBuilder extends BatchInsertSqlBuilder {

        public PostgresqlUpsertBatchInsertSqlBuilder(RDBTableMetadata table) {
            super(table);
        }

        @Override
        protected PrepareSqlFragments afterBuild(Set<InsertColumn> columns, InsertOperatorParameter parameter, PrepareSqlFragments sql) {
            sql.addSql("on conflict (", idColumn.getName(), ") do ");

            if (((PostgresqlUpsertOperatorParameter) parameter).doNoThingOnConflict) {
                sql.addSql("nothing");
                return sql;
            }


            int index = 0;
            boolean more = false;
            for (InsertColumn column : columns) {

                index++;
                if (column instanceof UpsertColumn && ((UpsertColumn) column).isUpdateIgnore()) {
                    continue;
                }
                RDBColumnMetadata columnMetadata = table.getColumn(column.getColumn()).orElse(null);
                if ( columnMetadata == null
                        || columnMetadata.isPrimaryKey()
                        || !columnMetadata.isUpdatable()
                        || !columnMetadata.isSaveable()) {

                    continue;
                }
                if (more) {
                    sql.addSql(",");
                } else {
                    sql.addSql("update set");
                }
                more = true;
                sql.addSql(columnMetadata.getQuoteName()).addSql("=");

                sql.addSql(
                        "coalesce(", columnMetadata.getFullName("excluded"), ",", columnMetadata.getFullName(),")"
                );
               // sql.addSql(columnMetadata.getFullName("excluded"));
            }
            if (!more) {
                sql.addSql("nothing");
            } else {
                // FIXME: 2021/4/15 实现类似 table._time>excluded._time的条件控制功能
                List<Term> where = ((PostgresqlUpsertOperatorParameter) parameter).where;
                if (CollectionUtils.isNotEmpty(where)) {
                    SqlFragments fragments = SimpleTermsFragmentBuilder.instance().createTermFragments(table, where);
                    if (fragments.isNotEmpty()) {
                        sql.addSql("where").addFragments(fragments);
                    }
                }
            }
            return sql;
        }

    }
}
