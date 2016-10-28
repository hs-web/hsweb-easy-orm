package org.hsweb.ezorm.rdb.render.dialect;

import org.hsweb.ezorm.rdb.meta.RDBDatabaseMetaData;
import org.hsweb.ezorm.rdb.render.Dialect;
import org.hsweb.ezorm.rdb.render.SqlRender;
import org.hsweb.ezorm.rdb.render.support.simple.*;

import java.util.HashMap;
import java.util.Map;

public abstract class AbstractDatabaseMeta extends RDBDatabaseMetaData {
    protected Map<SqlRender.TYPE, SqlRender> renderMap = new HashMap<>();
    protected Dialect dialect;

    public AbstractDatabaseMeta(Dialect dialect) {
        this.dialect = dialect;
    }

    public void init() {
        renderMap.put(SqlRender.TYPE.DELETE, new SimpleDeleteSqlRender(getDialect()));
        renderMap.put(SqlRender.TYPE.INSERT, new SimpleInsertSqlRender());
        renderMap.put(SqlRender.TYPE.SELECT, new SimpleSelectSqlRender(getDialect()));
        renderMap.put(SqlRender.TYPE.UPDATE, new SimpleUpdateSqlRender(getDialect()));
        renderMap.put(SqlRender.TYPE.SELECT_TOTAL, new SimpleSelectTotalSqlRender(getDialect()));
    }

    @Override
    public Dialect getDialect() {
        return dialect;
    }

    public SqlRender getRenderer(SqlRender.TYPE type) {
        SqlRender render = renderMap.get(type);
        if (render == null) throw new UnsupportedOperationException(type + " is not support");
        return render;
    }
}
