package org.hswebframework.ezorm.rdb.supports.oracle;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.rdb.executor.DefaultBatchSqlRequest;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatureType;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.EmptySqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.function.FunctionFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.insert.InsertSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertColumn;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperatorParameter;

import java.util.*;

import static java.util.Optional.ofNullable;

@SuppressWarnings("all")
@AllArgsConstructor(staticName = "of")
public class OracleInsertSqlBuilder implements InsertSqlBuilder {
    private RDBTableMetadata table;

    @Override
    public SqlRequest build(InsertOperatorParameter parameter) {
        PrepareSqlFragments fragments = PrepareSqlFragments.of();


        Map<Integer, RDBColumnMetadata> indexMapping = new LinkedHashMap<>();
        Map<Integer, SqlFragments> functionValues = new LinkedHashMap<>();

        int index = 0;
        int primaryIndex = -1;
        Set<InsertColumn> columns = parameter.getColumns();
        for (InsertColumn column : columns) {
            RDBColumnMetadata columnMetadata = ofNullable(column.getColumn())
                    .flatMap(table::getColumn)
                    .orElse(null);

            if (columnMetadata != null && columnMetadata.isInsertable()) {
                if (columnMetadata.isPrimaryKey()) {
                    primaryIndex = index;
                }
                indexMapping.put(index, columnMetadata);
                //列为函数
                SqlFragments functionFragments = Optional.of(column)
                        .flatMap(insertColumn -> Optional.ofNullable(insertColumn.getFunction())
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
        boolean batch = parameter.getValues().size() > 1;

        if (batch) {
            fragments.addSql("insert all");
        } else {
            fragments.addSql("insert");
        }

        for (List<Object> values : parameter.getValues()) {
            PrepareSqlFragments intoSql = PrepareSqlFragments.of();
            PrepareSqlFragments valuesSql = PrepareSqlFragments.of();

            intoSql.addSql("into")
                    .addSql(table.getFullName())
                    .addSql("(");

            valuesSql.addSql("values (");
            int valueLen = values.size();
            int vIndex = 0;
            Set<Object> duplicatePrimary = new HashSet<>();

            for (Map.Entry<Integer, RDBColumnMetadata> entry : indexMapping.entrySet()) {
                RDBColumnMetadata column = entry.getValue();
                int valueIndex = entry.getKey();
                if (primaryIndex >= 0) {
                    //重复的id 则不进行处理
                    if (values.size()>primaryIndex && !duplicatePrimary.add(values.get(primaryIndex))) {
                        continue;
                    }
                }
                if (vIndex++ != 0) {
                    intoSql.addSql(",");
                    valuesSql.addSql(",");
                }
                SqlFragments function = functionValues.get(valueIndex);
                if (null != function) {
                    valuesSql.addFragments(function);
                    continue;
                }

                Object value = valueLen <= valueIndex ? null : values.get(valueIndex);

                if ((value == null || value instanceof NullValue)
                        && column.getDefaultValue() instanceof RuntimeDefaultValue) {
                    value = ((RuntimeDefaultValue) column.getDefaultValue()).get();
                }
                intoSql.addSql(column.getQuoteName());

                if (value instanceof NativeSql) {
                    valuesSql.addSql(((NativeSql) value).getSql())
                            .addParameter(((NativeSql) value).getParameters());
                    continue;
                }
                if (value == null) {
                    value = NullValue.of(column.getType());
                }
                valuesSql.addSql("?").addParameter(column.encode(value));
            }
            intoSql.addSql(")");
            valuesSql.addSql(")");

            fragments.addFragments(intoSql)
                    .addFragments(valuesSql);
        }
        if (batch) {
            fragments.addSql("select 1 from dual");
        }

        return fragments.toRequest();
    }
}
