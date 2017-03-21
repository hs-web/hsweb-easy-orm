package org.hsweb.ezorm.rdb.render.dialect;

/**
 * TODO 完成注释
 *
 * @author zhouhao
 */
public class MSSQLDialect extends DefaultDialect {
    @Override
    public String getQuoteStart() {
        return "[";
    }

    @Override
    public String getQuoteEnd() {
        return "]";
    }

    @Override
    public String doPaging(String sql, int pageIndex, int pageSize) {
        return null;
    }

    @Override
    public boolean columnToUpperCase() {
        return false;
    }
}
