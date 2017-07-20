package org.hsweb.ezorm.rdb.meta.parser;

import org.hsweb.ezorm.rdb.executor.SqlExecutor;
import org.hsweb.ezorm.rdb.render.dialect.Dialect;
import org.hsweb.ezorm.rdb.render.dialect.MSSQLDialect;

import java.sql.JDBCType;

/**
 * @author zhouhao
 */
public class SqlServer2012TableMetaParser extends AbstractTableMetaParser {
    public SqlServer2012TableMetaParser(SqlExecutor sqlExecutor) {
        super(sqlExecutor);
        jdbcTypeMap.put("bigint", JDBCType.BIGINT);
        jdbcTypeMap.put("binary", JDBCType.BINARY);
        jdbcTypeMap.put("bit", JDBCType.BIT);
        jdbcTypeMap.put("char", JDBCType.CHAR);
        jdbcTypeMap.put("datetime", JDBCType.TIMESTAMP);
        jdbcTypeMap.put("decimal", JDBCType.DECIMAL);
        jdbcTypeMap.put("float", JDBCType.FLOAT);
        jdbcTypeMap.put("image", JDBCType.LONGVARBINARY);
        jdbcTypeMap.put("int", JDBCType.INTEGER);
        jdbcTypeMap.put("money", JDBCType.DECIMAL);
        jdbcTypeMap.put("nchar", JDBCType.CHAR);
        jdbcTypeMap.put("ntext", JDBCType.LONGVARCHAR);
        jdbcTypeMap.put("numeric", JDBCType.NUMERIC);
        jdbcTypeMap.put("nvarchar", JDBCType.VARCHAR);
        jdbcTypeMap.put("real", JDBCType.REAL);
        jdbcTypeMap.put("smalldatetime", JDBCType.TIMESTAMP);
        jdbcTypeMap.put("smallint", JDBCType.SMALLINT);
        jdbcTypeMap.put("smallmoney", JDBCType.DECIMAL);
        jdbcTypeMap.put("sql_variant", JDBCType.VARCHAR);
        jdbcTypeMap.put("sysname", JDBCType.VARCHAR);
        jdbcTypeMap.put("text", JDBCType.LONGVARCHAR);
        jdbcTypeMap.put("timestamp", JDBCType.BINARY);
        jdbcTypeMap.put("tinyint", JDBCType.TINYINT);
        jdbcTypeMap.put("uniqueidentifier", JDBCType.CHAR);
        jdbcTypeMap.put("varbinary", JDBCType.VARBINARY);
        jdbcTypeMap.put("varchar", JDBCType.VARCHAR);
    }

    static String TABLE_META_SQL = "SELECT \n" +
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
    Dialect getDialect() {
        return MSSQLDialect.MSSQL;
    }

    @Override
    String getTableMetaSql(String tname) {
        return TABLE_META_SQL;
    }

    @Override
    String getTableCommentSql(String tname) {
        return "select cast(p.value as varchar(500)) as comment from sys.extended_properties p " +
                " where p.major_id=object_id(#{table}) and p.minor_id=0";
    }

    @Override
    String getAllTableSql() {
        return "select name from sysobjects where xtype='U'";
    }

    @Override
    String getTableExistsSql() {
        return "select count(1) as total from sysobjects where xtype='U' and name = #{table}";
    }
}
