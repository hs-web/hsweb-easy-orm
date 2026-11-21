package org.hswebframework.ezorm.rdb.supports.kingbaseES.mssql;

import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.LiteralDataType;
import org.hswebframework.ezorm.rdb.supports.mssql.SqlServerDialect;

import java.sql.JDBCType;

/** SQL Server rendering with aliases exposed by the KingbaseES catalog. */
public class KingbaseSqlServerDialect extends SqlServerDialect {

    public KingbaseSqlServerDialect() {
        registerDataType("nvarchar", DataType.builder(JdbcDataType.of(JDBCType.NVARCHAR, String.class),
            column -> "nvarchar(" + column.getLength(255) + ")"));
        registerDataType("nchar", DataType.builder(JdbcDataType.of(JDBCType.CHAR, String.class),
            column -> "nchar(" + column.getLength(255) + ")"));
        registerDataType("varbinary", DataType.builder(JdbcDataType.of(JDBCType.VARBINARY, byte[].class),
            column -> "varbinary(" + column.getLength(255) + ")"));
        registerDataType("character varying", convertDataType("varchar"));
        registerDataType("character", convertDataType("char"));
        registerDataType("integer", convertDataType("int"));
        registerDataType("timestamp without time zone", convertDataType("datetime2"));
        registerDataType("bytea", DataType.builder(convertDataType("varbinary"), column -> "varbinary(max)"));
    }

    @Override
    protected DataType convertParameterizedDataType(String dataType, String type) {
        if (dataType.substring(dataType.indexOf('(') + 1, dataType.lastIndexOf(')')).trim().equalsIgnoreCase("max")
            && ("varchar".equals(type) || "nvarchar".equals(type) || "varbinary".equals(type))) {
            DataType base = convertDataType(type);
            return LiteralDataType.of(type, type + "(max)", base.getSqlType(), base.getJavaType(), false, false);
        }
        return super.convertParameterizedDataType(dataType, type);
    }

    @Override
    public String getId() {
        return "kingbase-sqlserver";
    }

    @Override
    public String getName() {
        return "KingbaseSqlServer";
    }
}
