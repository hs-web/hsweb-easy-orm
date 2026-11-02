package org.hswebframework.ezorm.rdb.supports.mssql;

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

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Supplier;
import java.util.stream.Collectors;

@SuppressWarnings("all")
public class SqlServerBatchUpsertOperator implements SaveOrUpdateOperator {

    private final RDBTableMetadata table;

    private final UpsertBatchInsertSqlBuilder builder;

    private RDBColumnMetadata idColumn;

    public SqlServerBatchUpsertOperator(RDBTableMetadata table) {
        this.table = table;
        this.builder = new UpsertBatchInsertSqlBuilder(table);
        this.idColumn = table.getColumns()
                             .stream().filter(RDBColumnMetadata::isPrimaryKey)
                             .findFirst().orElse(null);
    }

    @Override
    public SaveResultOperator execute(org.hswebframework.ezorm.rdb.operator.dml.upsert.UpsertOperatorParameter parameter) {
        if (idColumn == null) {
            this.idColumn = table
                .getColumns()
                .stream()
                .filter(RDBColumnMetadata::isPrimaryKey)
                .findFirst()
                .orElse(null);

            if (this.idColumn == null) {
                InsertOperatorParameter insertParameter = createInsertParameter(parameter, -1);
                insertParameter.setValues(parameter.getValues());
                return new InsertResultOperatorImpl(() -> createInsertSql(insertParameter));
            }
        }

        UpsertParameterSplit split = splitParameter(parameter);

        if (split.upsertParameter.getValues().isEmpty()) {
            return new InsertResultOperatorImpl(() -> createInsertSql(split.insertParameter));
        }
        if (split.insertParameter.getValues().isEmpty()) {
            return new SaveResultOperatorImpl(() -> builder.build(new UpsertOperatorParameter(split.upsertParameter)));
        }

        return new InsertAndUpsertResultOperatorImpl(
            () -> createInsertSql(split.insertParameter),
            () -> builder.build(new UpsertOperatorParameter(split.upsertParameter)));
    }

    private UpsertParameterSplit splitParameter(org.hswebframework.ezorm.rdb.operator.dml.upsert.UpsertOperatorParameter parameter) {
        int idIndex = indexOfIdColumn(parameter.getColumns());
        org.hswebframework.ezorm.rdb.operator.dml.upsert.UpsertOperatorParameter upsertParameter =
            new org.hswebframework.ezorm.rdb.operator.dml.upsert.UpsertOperatorParameter();
        upsertParameter.setColumns(new LinkedHashSet<>(parameter.getColumns()));
        upsertParameter.setWhere(parameter.getWhere());
        upsertParameter.setDoNothingOnConflict(parameter.isDoNothingOnConflict());

        InsertOperatorParameter insertParameter = createInsertParameter(parameter, idIndex);

        for (List<Object> values : parameter.getValues()) {
            if (hasIdValue(values, idIndex)) {
                upsertParameter.getValues().add(values);
            } else {
                insertParameter.getValues().add(createInsertValues(values, idIndex));
            }
        }
        return new UpsertParameterSplit(insertParameter, upsertParameter);
    }

    private int indexOfIdColumn(Set<UpsertColumn> columns) {
        if (idColumn == null) {
            return -1;
        }
        int index = 0;
        for (UpsertColumn column : columns) {
            if (idColumn.equalsNameOrAlias(column.getColumn())) {
                return index;
            }
            index++;
        }
        return -1;
    }

    private boolean hasIdValue(List<Object> values, int idIndex) {
        return idIndex >= 0
            && values.size() > idIndex
            && values.get(idIndex) != null
            && !(values.get(idIndex) instanceof NullValue);
    }

    private InsertOperatorParameter createInsertParameter(
        org.hswebframework.ezorm.rdb.operator.dml.upsert.UpsertOperatorParameter parameter,
        int idIndex) {
        InsertOperatorParameter insertParameter = new InsertOperatorParameter();
        boolean keepRuntimeDefaultId = useRuntimeDefaultId();
        if (idIndex < 0 && keepRuntimeDefaultId) {
            insertParameter.getColumns().add(InsertColumn.of(idColumn.getName()));
        }
        int index = 0;
        for (UpsertColumn column : parameter.getColumns()) {
            if (index++ == idIndex && !keepRuntimeDefaultId) {
                continue;
            }
            insertParameter.getColumns().add(column);
        }
        return insertParameter;
    }

    private List<Object> createInsertValues(List<Object> values, int idIndex) {
        if (!useRuntimeDefaultId()) {
            return removeValue(values, idIndex);
        }
        if (idIndex >= 0) {
            List<Object> newValues = new ArrayList<>(Math.max(values.size(), idIndex + 1));
            newValues.addAll(values);
            while (newValues.size() <= idIndex) {
                newValues.add(null);
            }
            if (newValues.get(idIndex) == null || newValues.get(idIndex) instanceof NullValue) {
                newValues.set(idIndex, createRuntimeDefaultId());
            }
            return newValues;
        }
        List<Object> newValues = new ArrayList<>(values.size() + 1);
        newValues.add(createRuntimeDefaultId());
        newValues.addAll(values);
        return newValues;
    }

    private boolean useRuntimeDefaultId() {
        return idColumn != null && idColumn.getDefaultValue() instanceof RuntimeDefaultValue;
    }

    private Object createRuntimeDefaultId() {
        return ((RuntimeDefaultValue) idColumn.getDefaultValue()).get();
    }

    private List<Object> removeValue(List<Object> values, int idIndex) {
        if (idIndex < 0 || values.size() <= idIndex) {
            return values;
        }
        List<Object> newValues = new ArrayList<>(values.size() - 1);
        for (int i = 0; i < values.size(); i++) {
            if (i != idIndex) {
                newValues.add(values.get(i));
            }
        }
        return newValues;
    }

    private SqlRequest createInsertSql(InsertOperatorParameter insertParameter) {
        return table
            .findFeatureNow(InsertSqlBuilder.ID)
            .build(insertParameter);
    }

    @AllArgsConstructor
    private class UpsertParameterSplit {

        private InsertOperatorParameter insertParameter;

        private org.hswebframework.ezorm.rdb.operator.dml.upsert.UpsertOperatorParameter upsertParameter;
    }

    class UpsertOperatorParameter extends InsertOperatorParameter {

        private boolean doNoThingOnConflict;

        private List<Term> where;

        public UpsertOperatorParameter(org.hswebframework.ezorm.rdb.operator.dml.upsert.UpsertOperatorParameter parameter) {
            doNoThingOnConflict = parameter.isDoNothingOnConflict();
            setColumns(parameter.toInsertColumns());
            setValues(parameter.getValues());
            where = parameter.getWhere();
        }

    }

    @AllArgsConstructor
    private class SaveResultOperatorImpl implements SaveResultOperator {

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

    @AllArgsConstructor
    private class InsertResultOperatorImpl implements SaveResultOperator {

        Supplier<SqlRequest> sqlRequest;

        @Override
        public SaveResult sync() {
            return ExceptionUtils.translation(() -> {
                SyncSqlExecutor sqlExecutor = table.findFeatureNow(SyncSqlExecutor.ID);
                int inserted = sqlExecutor.update(sqlRequest.get());
                return SaveResult.of(inserted, 0);
            }, table);
        }

        @Override
        public Mono<SaveResult> reactive() {
            return Mono
                .fromSupplier(sqlRequest)
                .as(table.findFeatureNow(ReactiveSqlExecutor.ID)::update)
                .map(i -> SaveResult.of(i, 0))
                .as(ExceptionUtils.translation(table));
        }
    }

    @AllArgsConstructor
    private class InsertAndUpsertResultOperatorImpl implements SaveResultOperator {

        Supplier<SqlRequest> insertRequest;

        Supplier<SqlRequest> upsertRequest;

        @Override
        public SaveResult sync() {
            return ExceptionUtils.translation(() -> {
                SyncSqlExecutor sqlExecutor = table.findFeatureNow(SyncSqlExecutor.ID);
                int inserted = sqlExecutor.update(insertRequest.get());
                int updated = sqlExecutor.update(upsertRequest.get());
                return SaveResult.of(inserted, updated);
            }, table);
        }

        @Override
        public Mono<SaveResult> reactive() {
            ReactiveSqlExecutor sqlExecutor = table.findFeatureNow(ReactiveSqlExecutor.ID);
            return Mono
                .fromSupplier(insertRequest)
                .as(sqlExecutor::update)
                .flatMap(inserted -> Mono
                    .fromSupplier(upsertRequest)
                    .as(sqlExecutor::update)
                    .map(updated -> SaveResult.of(inserted, updated)))
                .as(ExceptionUtils.translation(table));
        }
    }

    private class UpsertBatchInsertSqlBuilder implements InsertSqlBuilder {

        private final RDBTableMetadata table;

        public UpsertBatchInsertSqlBuilder(RDBTableMetadata table) {
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
                PREFIX = SqlFragments.of("merge into", table.getQuoteName(), "with(rowlock) as t using ( values");
            }
            UpsertOperatorParameter upsertParameter = (UpsertOperatorParameter) parameter;
            BatchSqlFragments fragments = new BatchSqlFragments();
            fragments.add(PREFIX);

            Map<Integer, Tuple2<RDBColumnMetadata, UpsertColumn>> columnMapping = createColumnIndex(parameter.getColumns());
            boolean notContainsId = true;
            int rowIndex = 0;
            for (List<Object> values : parameter.getValues()) {
                int valueIndex = 0;
                if (rowIndex > 0) {
                    fragments.add(SqlFragments.COMMA);
                }
                fragments.add(SqlFragments.LEFT_BRACKET);

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

                    fragments.add(SqlFragments.QUESTION_MARK).addParameter(column.encode(value));
                    valueIndex++;
                }

                if (notContainsId) {
                    if (idColumn.getDefaultValue() == null) {
                        throw new UnsupportedOperationException("column " + idColumn.getFullName() + " unsupported default value");
                    }
                    Object value = idColumn.getDefaultValue().get();
                    fragments.add(SqlFragments.COMMA);

                    if (value instanceof NativeSql) {
                        fragments
                            .addSql(((NativeSql) value).getSql())
                            .addParameter(((NativeSql) value).getParameters());
                    } else {
                        fragments.add(SqlFragments.QUESTION_MARK).addParameter(value);
                    }
                }
                fragments.add(SqlFragments.RIGHT_BRACKET);
                rowIndex++;
            }

            String columnStr = columnMapping.values()
                                            .stream()
                                            .map(tp2 -> tp2.getT1().getQuoteName())
                                            .collect(Collectors.joining(","));

            fragments.addSql(") as t2 (", columnStr, ") on (", idColumn.getFullName("t"), "=", idColumn.getFullName("t2"), ")");

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
                                  "=", "coalesce(" + t2Column + "," + tColumn + ")");

                    updateIndex++;
                }
            }

            if (update.isNotEmpty() && !upsertParameter.doNoThingOnConflict) {
                fragments.addSql("when matched then update set");
                fragments.addFragments(update);
            }


            fragments.addSql("when not matched then insert (");
            fragments.addFragments(insertColumns);
            fragments.addSql(") values (");
            fragments.addFragments(insertValues);
            fragments.addSql(");");

            return fragments.toRequest();
        }
    }
}
