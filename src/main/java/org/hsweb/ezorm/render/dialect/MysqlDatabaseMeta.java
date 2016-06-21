package org.hsweb.ezorm.render.dialect;

import org.hsweb.ezorm.render.Dialect;
import org.hsweb.ezorm.render.SqlRender;
import org.hsweb.ezorm.render.support.mysql.MysqlDeleteSqlRender;
import org.hsweb.ezorm.render.support.mysql.MysqlMetaAlterRender;
import org.hsweb.ezorm.render.support.mysql.MysqlMetaCreateRender;

public class MysqlDatabaseMeta extends AbstractDatabaseMeta {
    private static final String DEFAULT_NAME = "mysql";

    private String name;

    @Override
    public void init() {
        super.init();
        renderMap.put(SqlRender.TYPE.META_CREATE, new MysqlMetaCreateRender());
        renderMap.put(SqlRender.TYPE.DELETE, new MysqlDeleteSqlRender(Dialect.MYSQL));
        renderMap.put(SqlRender.TYPE.META_ALTER, new MysqlMetaAlterRender(this));
    }

    public MysqlDatabaseMeta() {
        name = DEFAULT_NAME;
        init();
    }

    public MysqlDatabaseMeta(String name) {
        this.name = name;
    }

    @Override
    public Dialect getDialect() {
        return Dialect.MYSQL;
    }

    @Override
    public String getName() {
        return name;
    }
}
