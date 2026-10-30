package org.hswebframework.ezorm.rdb.supports.kingbase.mysql;

import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlDialect;

import java.sql.JDBCType;

/**
 * MySQL dialect with the character type name returned by Kingbase metadata.
 */
public class KingbaseMysqlDialect extends MysqlDialect {

    public KingbaseMysqlDialect() {
        registerDataType("character varying", DataType.builder(
            JdbcDataType.of(JDBCType.VARCHAR, String.class),
            column -> "varchar(" + column.getLength(255) + ")"
        ));
    }

    @Override
    public String getId() {
        return "kingbase-mysql";
    }

    @Override
    public String getName() {
        return "KingbaseMysql";
    }
}
