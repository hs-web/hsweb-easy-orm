package org.hswebframework.ezorm.rdb.supports.postgres;

import lombok.AllArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.defaults.SaveResult;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
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
import java.util.Objects;
import java.util.Set;
import java.util.function.Supplier;
import java.util.stream.Collectors;

@SuppressWarnings("all")
public class PostgresqlBatchUpsertOperator implements SaveOrUpdateOperator {

    private RDBTableMetadata table;

    private PostgresqlUpsertBatchInsertSqlBuilder builder;

    private SqlFragments prefix;

    private SaveOrUpdateOperator fallback;

    public PostgresqlBatchUpsertOperator(RDBTableMetadata table) {
        this.table = table;
        this.fallback = new DefaultSaveOrUpdateOperator(table);
        this.builder = new PostgresqlUpsertBatchInsertSqlBuilder(table);
    }

    @Override
    public SaveResultOperator execute(UpsertOperatorParameter parameter) {
        if (getOrCreateOnConflict().isEmpty()) {
            return fallback.execute(parameter);
        }
        return new PostgresqlSaveResultOperator(() -> builder.build(new PostgresqlUpsertOperatorParameter(parameter)));
    }

    SqlFragments getOrCreateOnConflict() {
        if (prefix == null) {
            prefix = createOnConflict();
        }
        return prefix;
    }

    SqlFragments createOnConflict() {
        RDBColumnMetadata idColumn = table
            .getColumns()
            .stream()
            .filter(RDBColumnMetadata::isPrimaryKey)
            .findFirst()
            .orElse(null);
        if (idColumn != null) {
            return SqlFragments.of("on conflict (", idColumn.getName(), ") do ");
        }
        RDBIndexMetadata indexMetadata = table
            .getIndexes()
            .stream()
            .filter(index -> index.isUnique())
            .findFirst()
            .orElse(null);

        if (indexMetadata != null) {
            String columns = indexMetadata
                .getColumns()
                .stream()
                .map(c -> table.getColumn(c.getColumn()).orElse(null))
                .filter(Objects::nonNull)
                .map(RDBColumnMetadata::getQuoteName)
                .collect(Collectors.joining(","));

            return SqlFragments.of("on conflict( ", columns, ") do ");
        }
        return EmptySqlFragments.INSTANCE;
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

    static SqlFragments UPDATE_SET = SqlFragments.of("update set");

    private class PostgresqlUpsertBatchInsertSqlBuilder extends BatchInsertSqlBuilder {


        public PostgresqlUpsertBatchInsertSqlBuilder(RDBTableMetadata table) {
            super(table);
        }

        @Override
        protected int computeSqlSize(int columnSize, int valueSize) {
            return super.computeSqlSize(columnSize, valueSize) + columnSize * 3 + 2;
        }

        @Override
        protected AppendableSqlFragments afterBuild(Set<InsertColumn> columns, InsertOperatorParameter parameter, AppendableSqlFragments sql) {
            sql.add(createOnConflict());

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
                if (columnMetadata == null
                    || columnMetadata.isPrimaryKey()
                    || !columnMetadata.isUpdatable()
                    || !columnMetadata.isSaveable()) {

                    continue;
                }
                if (more) {
                    sql.add(SqlFragments.COMMA);
                } else {
                    sql.add(UPDATE_SET);
                }
                more = true;
                sql.addSql(columnMetadata.getQuoteName())
                   .add(SqlFragments.EQUAL);

                sql.addSql(
                    "coalesce(", columnMetadata.getFullName("excluded"), ",", columnMetadata.getFullName(), ")"
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
                        sql.add(SqlFragments.WHERE).addFragments(fragments);
                    }
                }
            }
            return sql;
        }

    }
}
