package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.rdb.dialect.Dialect;
import org.hswebframework.ezorm.rdb.render.SqlRender_;
import org.hswebframework.ezorm.rdb.render.dialect.AbstractRDBDatabaseMetaData;

public class MSSQLRDBDatabaseMetaData extends AbstractRDBDatabaseMetaData {
    private static final String DEFAULT_NAME = "mssql";

    private String name;

    @Override
    public void init() {
        super.init();
        putRenderer(SqlRender_.TYPE.META_CREATE, new SqlServerMetaCreateRender());
        putRenderer(SqlRender_.TYPE.META_ALTER, new SqlServerMetaAlterRender());
        putRenderer(SqlRender_.TYPE.SELECT, new SqlServerSelectSqlRender(getDialect()));
        putRenderer(SqlRender_.TYPE.DELETE, new SqlServerDeleteSqlRender(getDialect()));
        putRenderer(SqlRender_.TYPE.UPDATE, new SqlServerUpdateSqlRender(getDialect()));
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
