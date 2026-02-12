package org.hswebframework.ezorm.rdb.executor.reactive.r2dbc;

import org.hswebframework.ezorm.rdb.executor.PrepareSqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.utils.SqlUtils;

class R2dbcSqlRequest extends PrepareSqlRequest {

    private SqlRequest nativeSql;

    private String sql;

    private Object[] parameters;

    @Override
    public String getSql() {
        return sql;
    }

    @Override
    public Object[] getParameters() {
        return parameters;
    }

    @Override
    public boolean isEmpty() {
        return sql == null || sql.isEmpty();
    }

    @Override
    public String toNativeSql() {
        return SqlUtils.toNativeSql(nativeSql.getSql(), parameters);
    }

    public static SqlRequest of(int firstIndex, String symbol, SqlRequest request) {
        Object[] parameter = request.getParameters();
        // 没有预编译参数,直接返回.
        if (parameter == null || parameter.length == 0) {
            return request;
        }

        R2dbcSqlRequest sqlRequest = new R2dbcSqlRequest();
        sqlRequest.nativeSql = request;
        String sql = request.getSql();
        int sl = symbol.length() + 1;

        sqlRequest.sql = SqlUtils
            .replaceSqlParameter(
                sql,
                new StringBuilder(sql.length() + (parameter.length * sl)),
                (parameterIndex, builder) -> {
                    builder
                        .append(symbol)
                        .append((firstIndex + parameterIndex));
                })
            .toString();

        sqlRequest.parameters = parameter;

        return sqlRequest;
    }

}
