package org.hswebframework.ezorm.rdb.render.dialect;

import org.hswebframework.ezorm.rdb.render.SqlRender;
import org.hswebframework.ezorm.rdb.render.support.sqlserver.SqlServerDeleteSqlRender;
import org.hswebframework.ezorm.rdb.render.support.sqlserver.SqlServerMetaAlterRender;
import org.hswebframework.ezorm.rdb.render.support.sqlserver.SqlServerMetaCreateRender;
import org.hswebframework.ezorm.rdb.render.support.sqlserver.SqlServerSelectSqlRender;

public class MSSQLRDBDatabaseMetaData extends AbstractRDBDatabaseMetaData {
    private static final String DEFAULT_NAME = "mssql";

    private String name;

    @Override
    public void init() {
        super.init();
        putRenderer(SqlRender.TYPE.META_CREATE, new SqlServerMetaCreateRender());
        putRenderer(SqlRender.TYPE.META_ALTER, new SqlServerMetaAlterRender());
        putRenderer(SqlRender.TYPE.SELECT, new SqlServerSelectSqlRender(getDialect()));
        putRenderer(SqlRender.TYPE.DELETE, new SqlServerDeleteSqlRender(getDialect()));
    }

    public MSSQLRDBDatabaseMetaData() {
        super(Dialect.MSSQL);
        name = DEFAULT_NAME;
        init();
    }

    @Override
    public String getName() {
        return name;
    }
}
