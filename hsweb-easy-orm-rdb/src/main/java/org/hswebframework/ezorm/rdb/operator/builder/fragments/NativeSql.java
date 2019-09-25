package org.hswebframework.ezorm.rdb.operator.builder.fragments;

public interface NativeSql {
    String getSql();

    Object[] getParameters();

    static NativeSql of(String sql, Object... parameters) {
        return new NativeSql() {
            @Override
            public String getSql() {
                return sql;
            }

            @Override
            public Object[] getParameters() {
                return parameters;
            }
        };
    }
}
