package org.hswebframework.ezorm.rdb.supports.oracle;

import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.*;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.function.FunctionFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.insert.InsertSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertColumn;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperatorParameter;

import java.util.*;

import static java.util.Optional.ofNullable;

@SuppressWarnings("all")
@RequiredArgsConstructor(staticName = "of")
public class OracleInsertSqlBuilder implements InsertSqlBuilder {
    private final RDBTableMetadata table;

    private SqlFragments INTO_SQL;
    private static SqlFragments VALUES = SqlFragments.of("values ("),
        INSERT = SqlFragments.of("insert"),
        INSERT_ALL = SqlFragments.of("insert all"),
        FROM_DUAL = SqlFragments.of("select 1 from dual");

    @Override
    public SqlRequest build(InsertOperatorParameter parameter) {
        if (INTO_SQL == null) {
            INTO_SQL = SqlFragments.of("into", table.getFullName(), "(");
        }

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
        int valueSize = parameter.getValues().size();
        int columnSize = indexMapping.size();
        boolean batch = valueSize > 1;
        BatchSqlFragments fragments = new BatchSqlFragments(
            3 + valueSize * 2,
            valueSize * 2
        );

        if (batch) {
            fragments.add(INSERT_ALL);
        } else {
            fragments.add(INSERT);
        }
        Set<Object> duplicatePrimary = new HashSet<>();
        for (List<Object> values : parameter.getValues()) {
            BatchSqlFragments intoSql = new BatchSqlFragments(columnSize * 2 + 1, 0);
            BatchSqlFragments valuesSql = new BatchSqlFragments(columnSize * 2 + 1, columnSize);

            int valueLen = values.size();
            int vIndex = 0;

            if (primaryIndex >= 0) {
                //重复的id 则不进行处理
                if (values.size() > primaryIndex && !duplicatePrimary.add(values.get(primaryIndex))) {
                    continue;
                }
            }
            intoSql.add(INTO_SQL);

            valuesSql.add(VALUES);
            for (Map.Entry<Integer, RDBColumnMetadata> entry : indexMapping.entrySet()) {
                RDBColumnMetadata column = entry.getValue();
                int valueIndex = entry.getKey();
                if (vIndex++ != 0) {
                    intoSql.add(SqlFragments.COMMA);
                    valuesSql.add(SqlFragments.COMMA);
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
                valuesSql.add(SqlFragments.QUESTION_MARK).addParameter(column.encode(value));
            }
            intoSql.add(SqlFragments.RIGHT_BRACKET);
            valuesSql.add(SqlFragments.RIGHT_BRACKET);

            fragments.addFragments(intoSql)
                     .addFragments(valuesSql);
        }
        if (batch) {
            fragments.add(FROM_DUAL);
        }

        return fragments.toRequest();
    }
}
