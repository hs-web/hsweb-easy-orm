package org.hswebframework.ezorm.rdb.supports.oracle;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.defaults.SaveResult;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.BatchSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.insert.InsertSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertColumn;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperatorParameter;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.*;
import org.hswebframework.ezorm.rdb.utils.ExceptionUtils;
import reactor.core.publisher.Mono;
import reactor.util.function.Tuple2;
import reactor.util.function.Tuples;

import java.sql.JDBCType;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Supplier;

@SuppressWarnings("all")
public class OracleBatchUpsertOperator implements SaveOrUpdateOperator {

    private final RDBTableMetadata table;

    private final OracleUpsertBatchInsertSqlBuilder builder;

    private RDBColumnMetadata idColumn;

    private final SaveOrUpdateOperator fallback;

    public OracleBatchUpsertOperator(RDBTableMetadata table) {
        this.table = table;
        this.idColumn = table.getColumns()
                             .stream().filter(RDBColumnMetadata::isPrimaryKey)
                             .findFirst().orElse(null);
        this.fallback = new DefaultSaveOrUpdateOperator(table);
        this.builder = new OracleUpsertBatchInsertSqlBuilder(table);
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

        return new PostgresqlSaveResultOperator(() -> builder.build(new OracleUpsertOperatorParameter(parameter)));
    }

    class OracleUpsertOperatorParameter extends InsertOperatorParameter {

        private boolean doNoThingOnConflict;

        private List<Term> where;

        public OracleUpsertOperatorParameter(UpsertOperatorParameter parameter) {
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

    static SqlFragments UNION_ALL = SqlFragments.of("union all "),
        L_SELECT = SqlFragments.of("(select"),
        FROM_DUAL_R = SqlFragments.of("from dual) ");
    ;

    private class OracleUpsertBatchInsertSqlBuilder implements InsertSqlBuilder {

        private final RDBTableMetadata table;

        public OracleUpsertBatchInsertSqlBuilder(RDBTableMetadata table) {
            this.table = table;
        }

        private Map<Integer, Tuple2<RDBColumnMetadata, UpsertColumn>> createColumnIndex(Set<InsertColumn> columns) {
            Map<Integer, Tuple2<RDBColumnMetadata, UpsertColumn>> columnMapping = new LinkedHashMap<>(columns.size());
            int index = 0;
            for (InsertColumn column : columns) {

                RDBColumnMetadata metadata = table.getColumn(column.getColumn()).orElse(null);
                if (metadata == null) {
                    index++;
                    continue;
                }
                columnMapping.put(index++, Tuples.of(metadata, ((UpsertColumn) column)));
            }
            return columnMapping;
        }

        SqlFragments PREFIX;

        @Override
        public SqlRequest build(InsertOperatorParameter parameter) {
            if (PREFIX == null) {
                PREFIX = SqlFragments.of("merge into", table.getFullName(), "t using (");
            }
            OracleUpsertOperatorParameter upsertParameter = (OracleUpsertOperatorParameter) parameter;
            BatchSqlFragments fragments = new BatchSqlFragments();
            fragments.add(PREFIX);

            Map<Integer, Tuple2<RDBColumnMetadata, UpsertColumn>> columnMapping = createColumnIndex(parameter.getColumns());
            boolean notContainsId = true;
            int rowIndex = 0;
            for (List<Object> values : parameter.getValues()) {
                int valueIndex = 0;
                if (rowIndex > 0) {
                    fragments.add(UNION_ALL);
                }
                fragments.add(L_SELECT);

                for (Map.Entry<Integer, Tuple2<RDBColumnMetadata, UpsertColumn>> entry : columnMapping.entrySet()) {
                    int index = entry.getKey();
                    RDBColumnMetadata column = entry.getValue().getT1();
                    Object value = values.size() > index ? values.get(index) : null;
                    if (column.isPrimaryKey()) {
                        notContainsId = false;
                    }
                    if (valueIndex > 0) {
                        fragments.add(SqlFragments.COMMA);
                    }

                    if ((value == null || value instanceof NullValue)
                        && column.getDefaultValue() instanceof RuntimeDefaultValue) {
                        value = column.getDefaultValue().get();
                    }

                    if (value instanceof NativeSql) {
                        throw new UnsupportedOperationException("upsert unsupported NativeSql");
                    } else {
                        if (value == null) {
                            value = NullValue.of(column.getType());
                        }
                    }

                    fragments.addSql("? as ", column.getQuoteName())
                             .addParameter(column.encode(value));
                    valueIndex++;
                }

                if (notContainsId) {
                    if (idColumn.getDefaultValue() == null) {
                        throw new UnsupportedOperationException("column " + idColumn.getFullName() + " unsupported default value");
                    }
                    Object value = idColumn.getDefaultValue().get();
                    fragments.add(SqlFragments.COMMA);

                    if (value instanceof NativeSql) {
                        fragments.addSql(((NativeSql) value).getSql()).addParameter(((NativeSql) value).getParameters())
                                 .addSql("as", idColumn.getQuoteName());
                    } else {
                        fragments.addSql("? as", idColumn.getQuoteName()).addParameter(value);
                    }
                }
                fragments.add(FROM_DUAL_R);
                rowIndex++;
            }

            fragments.addSql(") t2 on (", idColumn.getFullName("t"), "=", idColumn.getFullName("t2"), ")");

            PrepareSqlFragments insertColumns = PrepareSqlFragments.of();
            PrepareSqlFragments insertValues = PrepareSqlFragments.of();
            PrepareSqlFragments update = PrepareSqlFragments.of();


            boolean ignoreNullColumn = parameter.getValues().size() == 1;
            List<Object> firstValues = parameter.getValues().get(0);
            int insertIndex = 0, insertValueIndex = 0, updateIndex = 0;

            if (notContainsId) {
                insertIndex = 1;
                insertColumns.addSql(idColumn.getQuoteName());
                insertValues.addSql(idColumn.getFullName("t2"));
            }

            for (Tuple2<RDBColumnMetadata, UpsertColumn> columnBind : columnMapping.values()) {
                RDBColumnMetadata column = columnBind.getT1();

                String t2Column = column.getFullName("t2");
                String tColumn = column.getFullName("t");

                //insert
                {
                    boolean canInsert = column.isInsertable();
                    if (canInsert && ignoreNullColumn) {
                        Object value = firstValues.size() > insertValueIndex ? firstValues.get(insertValueIndex) : null;
                        if (value == null || value instanceof NullValue) {
                            canInsert = false;
                        }
                    }
                    insertValueIndex++;

                    if (canInsert) {
                        if (insertIndex > 0) {
                            insertColumns.addSql(",");
                            insertValues.addSql(",");
                        }
                        insertColumns.addSql(column.getQuoteName());
                        insertValues.addSql(t2Column);
                        insertIndex++;
                    }
                }

                //update
                {
                    if (column.isPrimaryKey()
                        || !column.isUpdatable()
                        || !column.isSaveable()
                        || columnBind.getT2().isUpdateIgnore()) {

                        continue;
                    }
                    if (updateIndex > 0) {
                        update.addSql(",");
                    }
                    update.addSql(tColumn,
                                  "=", "NVL(" + t2Column + "," + tColumn + ")");

                    updateIndex++;
                }
            }

            if (update.isNotEmpty() || upsertParameter.doNoThingOnConflict) {
                fragments.addSql("when matched then update set");
                fragments.addFragments(update);
            }


            fragments.addSql("when not matched then insert (");
            fragments.addFragments(insertColumns);
            fragments.addSql(") values (");
            fragments.addFragments(insertValues);
            fragments.addSql(")");

            return fragments.toRequest();
        }
    }
}
