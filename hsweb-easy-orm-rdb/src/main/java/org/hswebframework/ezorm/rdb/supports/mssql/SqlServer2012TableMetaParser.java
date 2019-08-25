package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.supports.commons.RDBTableMetaParser;
import org.hswebframework.ezorm.rdb.dialect.Dialect;

/**
 * @author zhouhao
 */
public class SqlServer2012TableMetaParser extends RDBTableMetaParser {

    public SqlServer2012TableMetaParser(SyncSqlExecutor sqlExecutor) {
        super(sqlExecutor);
    }

    private static String TABLE_META_SQL = "SELECT \n" +
            "c.name as name,\n" +
            "t.name as data_type,\n" +
            "c.length as data_length,\n" +
            "c.xscale as data_scale,\n" +
            "c.xprec as data_precision,\n" +
            "case when c.isnullable=1 then 0 else  1 end as [not-null],\n" +
            "cast(p.value as varchar(500)) as comment\n" +
            "FROM syscolumns c\n" +
            "inner join  systypes t on c.xusertype = t.xusertype \n" +
            "left join sys.extended_properties p on c.id=p.major_id and c.colid=p.minor_id\n" +
            "WHERE c.id = object_id(#{table})";

    @Override
    protected Dialect getDialect() {
        return MSSQLDialect.MSSQL;
    }

    @Override
    protected String getTableMetaSql(String tname) {
        return TABLE_META_SQL;
    }

    @Override
    protected String getTableCommentSql(String tname) {
        return "select cast(p.value as varchar(500)) as comment from sys.extended_properties p " +
                " where p.major_id=object_id(#{table}) and p.minor_id=0";
    }

    @Override
    protected String getAllTableSql() {
        return "select name from sysobjects where xtype='U'";
    }

    @Override
    protected String getTableExistsSql() {
        return "select count(1) as total from sysobjects where xtype='U' and name = #{table}";
    }
}
