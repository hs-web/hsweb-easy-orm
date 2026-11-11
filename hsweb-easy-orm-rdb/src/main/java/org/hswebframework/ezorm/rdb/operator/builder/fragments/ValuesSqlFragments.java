package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;

import java.util.List;
import java.util.ArrayList;
import java.util.Collections;

/** 已编码行的标准 VALUES 表达式；供方言构造批量插入或关联值集使用。 */
public final class ValuesSqlFragments {

    private ValuesSqlFragments() {
    }

    public static SqlFragments of(List<RDBColumnMetadata> columns, List<Object[]> rows) {
        if (rows.isEmpty()) return EmptySqlFragments.INSTANCE;
        if (columns.isEmpty()) throw new IllegalArgumentException("No value columns");
        StringBuilder sql = new StringBuilder("values ");
        List<Object> parameters = new ArrayList<>(rows.size() * columns.size());
        for (int row = 0; row < rows.size(); row++) {
            Object[] values = rows.get(row);
            if (values.length != columns.size()) throw new IllegalArgumentException("Value count does not match columns");
            if (row > 0) sql.append(',');
            sql.append('(');
            for (int col = 0; col < columns.size(); col++) {
                if (col > 0) sql.append(',');
                Object value = values[col];
                if (value instanceof NativeSql nativeSql) {
                    sql.append(nativeSql.getSql());
                    Collections.addAll(parameters, nativeSql.getParameters());
                } else {
                    sql.append('?');
                    parameters.add(value == null ? NullValue.of(columns.get(col).getType()) : value);
                }
            }
            sql.append(')');
        }
        return SimpleSqlFragments.of(Collections.singletonList(sql.toString()), parameters);
    }
}
