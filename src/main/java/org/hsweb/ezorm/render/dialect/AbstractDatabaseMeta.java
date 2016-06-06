package org.hsweb.ezorm.render.dialect;

import org.hsweb.ezorm.meta.DatabaseMetaData;
import org.hsweb.ezorm.render.SqlRender;
import org.hsweb.ezorm.render.support.simple.*;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by zhouhao on 16-6-5.
 */
public abstract class AbstractDatabaseMeta extends DatabaseMetaData {
    protected Map<SqlRender.TYPE, SqlRender> renderMap = new HashMap<>();

    public void init() {
        renderMap.put(SqlRender.TYPE.DELETE, new SimpleDeleteSqlRender(getDialect()));
        renderMap.put(SqlRender.TYPE.INSERT, new SimpleInsertSqlRender());
        renderMap.put(SqlRender.TYPE.SELECT, new SimpleSelectSqlRender(getDialect()));
        renderMap.put(SqlRender.TYPE.UPDATE, new SimpleUpdateSqlRender(getDialect()));
        renderMap.put(SqlRender.TYPE.SELECT_TOTAL, new SimpleSelectTotalSqlRender(getDialect()));
    }

    public SqlRender getRenderer(SqlRender.TYPE type) {
        SqlRender render = renderMap.get(type);
        if (render == null) throw new UnsupportedOperationException(type + " is not support");
        return render;
    }
}
