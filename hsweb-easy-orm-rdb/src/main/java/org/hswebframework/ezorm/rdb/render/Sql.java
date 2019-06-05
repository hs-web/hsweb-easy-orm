package org.hswebframework.ezorm.rdb.render;

/**
 * @author zhouhao
 */
public interface Sql {
    String getSql();

    static Sql build(String sql) {
        return () -> sql;
    }
}
