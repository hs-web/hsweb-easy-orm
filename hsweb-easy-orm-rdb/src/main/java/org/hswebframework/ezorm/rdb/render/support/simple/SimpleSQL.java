package org.hswebframework.ezorm.rdb.render.support.simple;

import org.hswebframework.ezorm.rdb.executor.BindSQL;
import org.hswebframework.ezorm.rdb.executor.SQL;

import java.util.HashMap;
import java.util.List;

/**
 * Created by zhouhao on 16-6-4.
 */
public class SimpleSQL implements SQL {

    private String sql;

    private Object params;

    private List<BindSQL> bindSQLs;

    public SimpleSQL(String sql, Object params) {
        this.sql = sql;
        this.params = params;
    }

    public SimpleSQL(String sql) {
        this.sql = sql;
        this.params = new HashMap<>();
    }


    public void setParams(Object params) {
        this.params = params;
    }

    @Override
    public String getSql() {
        return sql;
    }

    @Override
    public Object getParams() {
        return params;
    }

    @Override
    public List<BindSQL> getBinds() {
        return bindSQLs;
    }

    public void setBindSQLs(List<BindSQL> bindSQLs) {
        this.bindSQLs = bindSQLs;
    }

    @Override
    public int size() {
        return bindSQLs == null ? 1 : bindSQLs.size() + 1;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append(sql).append("\n").append(params);
        return builder.toString();
    }
}
