package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import java.util.Collection;

public interface AppendableSqlFragments extends SqlFragments {

    AppendableSqlFragments addSql(String... sql);

    AppendableSqlFragments addSql(Collection<String> sql);

    AppendableSqlFragments addParameter(Object... parameter);

    AppendableSqlFragments addParameter(Collection<?> parameter);

    AppendableSqlFragments addFragments(SqlFragments fragments);

    default AppendableSqlFragments add(SqlFragments fragments) {
        return addFragments(fragments);
    }

    /**
     * 追加已编码的单值：表达式展开 SQL 和参数，普通值使用占位符。
     * 不再次编码，也不解释 Java null 的省略或默认值语义。
     *
     * @param value 已编码的值
     * @return 当前片段
     * @see SqlFragments#encodeValue
     * @since 4.2.3
     */
    default AppendableSqlFragments addValue(Object value) {
        if (value instanceof NativeSql) {
            NativeSql sql = (NativeSql) value;
            return addSql(sql.getSql()).addParameter(sql.getParameters());
        }
        return add(SqlFragments.QUESTION_MARK).addParameter(value);
    }

}
