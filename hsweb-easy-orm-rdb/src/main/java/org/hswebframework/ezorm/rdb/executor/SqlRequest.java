package org.hswebframework.ezorm.rdb.executor;

public interface SqlRequest {

    String getSql();

    Object[] getParameters();

    static SqlRequest of(String sql, Object... parameters) {
        return SimpleSqlRequest.of(sql, parameters);
    }

    static SqlRequest template(String template, Object parameter) {
        return SqlTemplateParser.parse(template, parameter);
    }

}
