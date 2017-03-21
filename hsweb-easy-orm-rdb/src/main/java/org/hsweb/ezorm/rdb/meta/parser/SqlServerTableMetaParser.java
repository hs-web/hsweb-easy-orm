package org.hsweb.ezorm.rdb.meta.parser;

import org.hsweb.ezorm.rdb.executor.SqlExecutor;
import org.hsweb.ezorm.rdb.render.dialect.Dialect;
import org.hsweb.ezorm.rdb.render.dialect.MSSQLDialect;

/**
 * TODO 完成注释
 *
 * @author zhouhao
 */
public class SqlServerTableMetaParser extends AbstractTableMetaParser {
    public SqlServerTableMetaParser(SqlExecutor sqlExecutor) {
        super(sqlExecutor);
    }

    @Override
    Dialect getDialect() {
        return MSSQLDialect.MSSQL;
    }

    @Override
    String getTableMetaSql(String tname) {
        return null;
    }

    @Override
    String getTableCommentSql(String tname) {
        return null;
    }

    @Override
    String getAllTableSql() {
        return null;
    }

    @Override
    String getTableExistsSql() {
        return null;
    }
}
