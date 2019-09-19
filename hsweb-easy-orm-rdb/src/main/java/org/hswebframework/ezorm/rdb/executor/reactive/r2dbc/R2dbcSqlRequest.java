package org.hswebframework.ezorm.rdb.executor.reactive.r2dbc;

import org.hswebframework.ezorm.rdb.executor.PrepareSqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.utils.SqlUtils;

public class R2dbcSqlRequest extends PrepareSqlRequest {

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
        return sql.isEmpty();
    }

    @Override
    public String toNativeSql() {
        return SqlUtils.toNativeSql(nativeSql.getSql(), parameters);
    }

    public static R2dbcSqlRequest of(int firstIndex, String symbol, SqlRequest request) {
        R2dbcSqlRequest sqlRequest = new R2dbcSqlRequest();
        sqlRequest.nativeSql = request;
        String sql = request.getSql();

        int len = 0;

        StringBuilder builder = new StringBuilder(sql.length() + len + 16);

        int parameterIndex = firstIndex;
        for (int i = 0, sqlLen = sql.length(); i < sqlLen; i++) {
            char c = sql.charAt(i);
            if (c == '?') {
                builder.append(symbol.concat(String.valueOf(parameterIndex++)));
            } else {
                builder.append(c);
            }
        }
        sqlRequest.sql = builder.toString();
        sqlRequest.parameters = request.getParameters();
        return sqlRequest;
    }

}
