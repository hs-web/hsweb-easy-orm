package org.hsweb.ezorm.render.support.simple;

import org.hsweb.ezorm.executor.BindSQL;
import org.hsweb.ezorm.executor.SQL;

import java.util.HashMap;
import java.util.List;

/**
 * Created by zhouhao on 16-6-4.
 */
public class SimpleSQL implements SQL {

    private String sql;

    private Object param;

    private List<BindSQL> bindSQLs;

    public SimpleSQL(String sql, Object param) {
        this.sql = sql;
        this.param = param;
    }

    public SimpleSQL(String sql) {
        this.sql = sql;
        this.param = new HashMap<>();
    }


    @Override
    public String getSql() {
        return sql;
    }

    @Override
    public Object getParams() {
        return param;
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
        builder.append(sql).append("\n").append(param);
        return builder.toString();
    }
}
