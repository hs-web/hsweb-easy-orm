package org.hswebframework.ezorm.rdb.render.dialect;

import org.hswebframework.ezorm.rdb.render.SqlRender;
import org.hswebframework.ezorm.rdb.render.support.mysql.MysqlDeleteSqlRender;
import org.hswebframework.ezorm.rdb.render.support.mysql.MysqlMetaAlterRender;
import org.hswebframework.ezorm.rdb.render.support.mysql.MysqlMetaCreateRender;

public class MysqlRDBDatabaseMetaData extends AbstractRDBDatabaseMetaData {
    private static final String DEFAULT_NAME = "mysql";

    private String name;

    @Override
    public void init() {
        super.init();
        renderMap.put(SqlRender.TYPE.META_CREATE, new MysqlMetaCreateRender());
        renderMap.put(SqlRender.TYPE.DELETE, new MysqlDeleteSqlRender(getDialect()));
        renderMap.put(SqlRender.TYPE.META_ALTER, new MysqlMetaAlterRender(this));
    }

    public MysqlRDBDatabaseMetaData() {
        super(Dialect.MYSQL);
        name = DEFAULT_NAME;
        init();
    }

    @Override
    public String getName() {
        return name;
    }
}
