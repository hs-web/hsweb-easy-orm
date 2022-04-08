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
        return sql.isEmpty();
    }

    @Override
    public String toNativeSql() {
        return SqlUtils.toNativeSql(nativeSql.getSql(), parameters);
    }

    private final static ThreadLocal<StringBuilder> builderRef = ThreadLocal.withInitial(StringBuilder::new);

    public static R2dbcSqlRequest of(int firstIndex, String symbol, SqlRequest request) {
        R2dbcSqlRequest sqlRequest = new R2dbcSqlRequest();
        sqlRequest.nativeSql = request;
        String sql = request.getSql();

        StringBuilder builder = builderRef.get();
        builder.setLength(0);

        int parameterIndex = firstIndex;
        for (int i = 0, sqlLen = sql.length(); i < sqlLen; i++) {
            char c = sql.charAt(i);
            //替换为 ?0,?1
            if (c == '?') {
                builder.append(symbol).append(parameterIndex++);
            } else {
                builder.append(c);
            }
        }
        sqlRequest.sql = builder.toString();
        sqlRequest.parameters = request.getParameters();

        return sqlRequest;
    }

}
