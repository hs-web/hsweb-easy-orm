package org.hswebframework.ezorm.rdb.executor;

/**
 * Created by 浩 on 2015-11-06 0006.
 */
public class BindSQL {
    private SQL sql;

    private String toField;

    public BindSQL() {
    }

    public BindSQL(SQL sql) {
        this.sql = sql;
    }

    public SQL getSql() {
        return sql;
    }

    public void setSql(SQL sql) {
        this.sql = sql;
    }

    public String getToField() {
        return toField;
    }

    public void setToField(String toField) {
        this.toField = toField;
    }
}
