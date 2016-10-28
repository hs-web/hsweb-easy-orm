package org.hsweb.ezorm.rdb.render.dialect;

import org.hsweb.ezorm.rdb.render.SqlRender;
import org.hsweb.ezorm.rdb.render.support.mysql.MysqlDeleteSqlRender;
import org.hsweb.ezorm.rdb.render.support.mysql.MysqlMetaAlterRender;
import org.hsweb.ezorm.rdb.render.support.mysql.MysqlMetaCreateRender;

public class MysqlDatabaseMeta extends AbstractDatabaseMeta {
    private static final String DEFAULT_NAME = "mysql";

    private String name;

    @Override
    public void init() {
        super.init();
        renderMap.put(SqlRender.TYPE.META_CREATE, new MysqlMetaCreateRender());
        renderMap.put(SqlRender.TYPE.DELETE, new MysqlDeleteSqlRender(getDialect()));
        renderMap.put(SqlRender.TYPE.META_ALTER, new MysqlMetaAlterRender(this));
    }

    public MysqlDatabaseMeta() {
        super(new MysqlDialect());
        name = DEFAULT_NAME;
        init();
    }

    @Override
    public String getName() {
        return name;
    }
}
