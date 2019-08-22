package org.hswebframework.ezorm.rdb.render.dialect;

import org.hswebframework.ezorm.rdb.dialect.Dialect;
import org.hswebframework.ezorm.rdb.meta.RDBDatabaseMetaData;
import org.hswebframework.ezorm.rdb.render.SqlRender;
import org.hswebframework.ezorm.rdb.supports.commons.*;

import java.util.HashMap;
import java.util.Map;

public abstract class AbstractRDBDatabaseMetaData extends RDBDatabaseMetaData {
    protected Map<SqlRender.TYPE, SqlRender> renderMap = new HashMap<>();
    protected Dialect dialect;

    public AbstractRDBDatabaseMetaData(Dialect dialect) {
        this.dialect = dialect;
    }

    public void init() {
        putRenderer(SqlRender.TYPE.DELETE, new SimpleDeleteSqlRender(getDialect()));
        putRenderer(SqlRender.TYPE.INSERT, new SimpleInsertSqlRender());
        putRenderer(SqlRender.TYPE.SELECT, new SimpleSelectSqlRender(getDialect()));
        putRenderer(SqlRender.TYPE.UPDATE, new SimpleUpdateSqlRender(getDialect()));
        putRenderer(SqlRender.TYPE.SELECT_TOTAL, new SimpleSelectTotalSqlRender(getDialect()));
    }

    @Override
    public Dialect getDialect() {
        return dialect;
    }

    public <T> SqlRender<T> getRenderer(SqlRender.TYPE type) {
        SqlRender<T> render = renderMap.get(type);
        if (render == null) throw new UnsupportedOperationException(type + " is not support");
        return render;
    }

    public void putRenderer(SqlRender.TYPE type, SqlRender sqlRender) {
        renderMap.put(type, sqlRender);
    }
}
