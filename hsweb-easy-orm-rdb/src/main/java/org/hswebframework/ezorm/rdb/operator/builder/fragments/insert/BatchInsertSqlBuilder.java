package org.hswebframework.ezorm.rdb.operator.builder.fragments.insert;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.*;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.EmptySqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.function.FunctionFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertColumn;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperatorParameter;

import java.util.*;

import static java.util.Optional.*;

@AllArgsConstructor
@SuppressWarnings("all")
public class BatchInsertSqlBuilder implements InsertSqlBuilder {

    protected RDBTableMetadata table;

    public static BatchInsertSqlBuilder of(RDBTableMetadata table) {
        return new BatchInsertSqlBuilder(table);
    }

    @Override
    public SqlRequest build(InsertOperatorParameter parameter) {
        PrepareSqlFragments fragments = beforeBuild(parameter, PrepareSqlFragments.of()).addSql("(");

        Map<Integer, RDBColumnMetadata> indexMapping = new LinkedHashMap<>();
        Map<Integer, SqlFragments> functionValues = new LinkedHashMap<>();

        int index = 0;
        Set<InsertColumn> columns = parameter.getColumns();

        for (InsertColumn column : columns) {
            RDBColumnMetadata columnMetadata = ofNullable(column.getColumn())
                    .flatMap(table::getColumn)
                    .orElse(null);

            if (columnMetadata != null && columnMetadata.isInsertable()) {
                if (indexMapping.size() != 0) {
                    fragments.addSql(",");
                }
                fragments.addSql(columnMetadata.getQuoteName());
                indexMapping.put(index, columnMetadata);
                //列为函数
                SqlFragments functionFragments = Optional.of(column)
                        .flatMap(insertColumn -> ofNullable(insertColumn.getFunction())
                                .flatMap(function -> columnMetadata.findFeature(FunctionFragmentBuilder.createFeatureId(function)))
                                .map(builder -> builder.create(columnMetadata.getName(), columnMetadata, insertColumn.getOpts())))
                        .orElse(EmptySqlFragments.INSTANCE);
                if (functionFragments.isNotEmpty()) {
                    functionValues.put(index, functionFragments);
                }
            }
            index++;
        }

        if (indexMapping.isEmpty()) {
            throw new IllegalArgumentException("No operable columns");
        }
        fragments.addSql(") values ");
        index = 0;
        for (List<Object> values : parameter.getValues()) {
            if (index++ != 0) {
                fragments.addSql(",");
            }
            fragments.addSql("(");
            int valueLen = values.size();
            int vIndex = 0;
            for (Map.Entry<Integer, RDBColumnMetadata> entry : indexMapping.entrySet()) {
                int valueIndex = entry.getKey();
                if (vIndex++ != 0) {
                    fragments.addSql(",");
                }
                SqlFragments function = functionValues.get(valueIndex);
                if (null != function) {
                    fragments.addFragments(function);
                    continue;
                }
                RDBColumnMetadata column = entry.getValue();

                Object value = valueLen <= valueIndex ? null : values.get(valueIndex);

                if ((value == null || value instanceof NullValue)
                        && column.getDefaultValue() instanceof RuntimeDefaultValue) {
                    value = column.getDefaultValue().get();
                }
                if (value instanceof NativeSql) {
                    fragments
                            .addSql(((NativeSql) value).getSql())
                            .addParameter(((NativeSql) value).getParameters());
                    continue;
                }
                if (value == null) {
                    value = NullValue.of(column.getType());
                }
                fragments.addSql("?").addParameter(column.encode(value));
            }

            fragments.addSql(")");
            afterValues(columns, values, fragments);
        }

        return afterBuild(columns, parameter, fragments).toRequest();
    }

    protected PrepareSqlFragments beforeBuild(InsertOperatorParameter parameter, PrepareSqlFragments fragments) {
        return fragments.addSql("insert into")
                .addSql(table.getFullName());
    }

    protected PrepareSqlFragments afterBuild(Set<InsertColumn> columns, InsertOperatorParameter parameter, PrepareSqlFragments fragments) {

        return fragments;
    }

    protected void afterValues(Set<InsertColumn> columns, List<Object> values, PrepareSqlFragments sql) {

    }


}
