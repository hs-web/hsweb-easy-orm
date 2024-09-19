package org.hswebframework.ezorm.rdb.operator.builder.fragments.insert;

import com.google.common.collect.Maps;
import lombok.AllArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.*;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.*;
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

    static SqlFragments VALUES = SqlFragments.single(") values ");

    protected int computeSqlSize(int columnSize, int valueSize) {
        return (columnSize * valueSize) * 2 + valueSize * 2 + columnSize * 3 + 2;
    }

    @Override
    public SqlRequest build(InsertOperatorParameter parameter) {
//        PrepareSqlFragments fragments = beforeBuild(parameter, PrepareSqlFragments.of()).addSql("(");
        Set<InsertColumn> columns = parameter.getColumns();
        List<List<Object>> valueList = parameter.getValues();
        int columnSize = columns.size();
        int valueSize = valueList.size();
        AppendableSqlFragments fragments = beforeBuild(
            parameter,
            new BatchSqlFragments(
                computeSqlSize(columnSize, valueSize),
                columnSize * valueSize
            ));

        fragments.add(SqlFragments.LEFT_BRACKET);

        LinkedHashMap<Integer, RDBColumnMetadata> indexMapping = Maps.newLinkedHashMapWithExpectedSize(columns.size());
        LinkedHashMap<Integer, SqlFragments> functionValues = Maps.newLinkedHashMapWithExpectedSize(columns.size());

        int index = 0;
        int primaryIndex = -1;

        //如果只有一条数据则忽略null的列
        boolean ignoreNullColumn = parameter.getValues().size() == 1;

        for (InsertColumn column : columns) {
            RDBColumnMetadata columnMetadata = ofNullable(column.getColumn())
                .flatMap(table::getColumn)
                .orElse(null);
            if (columnMetadata != null && columnMetadata.isInsertable()) {
                if (columnMetadata.isPrimaryKey()) {
                    primaryIndex = index;
                }
                //忽略null的列
                if (ignoreNullColumn) {
                    List<Object> values = parameter.getValues().get(0);
                    if (index >= values.size()
                        || values.get(index) instanceof NullValue
                        //为空并且没有默认值
                        || (values.get(index) == null && !(columnMetadata.getDefaultValue() instanceof RuntimeDefaultValue))) {
                        index++;
                        continue;
                    }
                }
                if (indexMapping.size() != 0) {
                    fragments.add(SqlFragments.COMMA);
                }
                fragments.addSql(columnMetadata.getQuoteName());
                indexMapping.put(index, columnMetadata);

                //列为函数
                if (StringUtils.isNotEmpty(column.getFunction())) {
                    functionValues.put(
                        index,
                        columnMetadata
                            .findFeatureNow(FunctionFragmentBuilder.createFeatureId(column.getFunction()))
                            .create(columnMetadata.getName(), columnMetadata, column));
                }
            }
            index++;
        }

        if (indexMapping.isEmpty()) {
            throw new IllegalArgumentException("No operable columns");
        }
        // ) values
        fragments.add(VALUES);
        index = 0;
        Set<Object> duplicatePrimary = new HashSet<>(32);
        for (List<Object> values : valueList) {
            if (primaryIndex >= 0) {
                //重复的id 则不进行处理
                if (values.size() > primaryIndex && !duplicatePrimary.add(values.get(primaryIndex))) {
                    continue;
                }
            }
            if (index++ != 0) {
                fragments.add(SqlFragments.COMMA);
            }
            fragments.add(SqlFragments.LEFT_BRACKET);
            int valueLen = values.size();
            int vIndex = 0;
            for (Map.Entry<Integer, RDBColumnMetadata> entry : indexMapping.entrySet()) {
                int valueIndex = entry.getKey();
                if (vIndex++ != 0) {
                    fragments.add(SqlFragments.COMMA);
                }
                SqlFragments function = functionValues.get(valueIndex);
                if (null != function) {
                    fragments.add(function);
                } else {
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

                    } else {
                        if (value == null) {
                            value = NullValue.of(column.getType());
                        }
                        value = column.encode(value);
                        if (value instanceof NativeSql) {
                            fragments
                                .addSql(((NativeSql) value).getSql())
                                .addParameter(((NativeSql) value).getParameters());

                        } else {
                            fragments.add(SqlFragments.QUESTION_MARK)
                                     .addParameter(value);
                        }
                    }
                }
            }

            fragments.add(SqlFragments.RIGHT_BRACKET);
            afterValues(columns, values, fragments);
        }

        return afterBuild(columns, parameter, fragments).toRequest();
    }

    protected static SqlFragments INSERT_INTO = SqlFragments.single("insert into");

    protected AppendableSqlFragments beforeBuild(InsertOperatorParameter parameter, AppendableSqlFragments fragments) {
        return fragments.add(INSERT_INTO)
                        .addSql(table.getFullName());
    }

    protected AppendableSqlFragments afterBuild(Set<InsertColumn> columns, InsertOperatorParameter parameter, AppendableSqlFragments fragments) {

        return fragments;
    }

    protected void afterValues(Set<InsertColumn> columns, List<Object> values, AppendableSqlFragments sql) {

    }


}
