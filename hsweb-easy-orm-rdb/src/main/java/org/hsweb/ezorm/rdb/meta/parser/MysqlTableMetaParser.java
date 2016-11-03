package org.hsweb.ezorm.rdb.meta.parser;

import org.hsweb.ezorm.rdb.executor.SqlExecutor;
import org.hsweb.ezorm.rdb.render.dialect.Dialect;
import org.hsweb.ezorm.rdb.render.dialect.MysqlDialect;

import java.sql.JDBCType;

/**
 * Created by zhouhao on 16-6-5.
 */
public class MysqlTableMetaParser extends AbstractTableMetaParser {
    String TABLE_META_SQL = " select " +
            "column_name as `name`, " +
            "data_type as `data_type`, " +
            "character_maximum_length as `data_length`, " +
            "numeric_precision as `data_precision`, " +
            "numeric_scale as `data_scale`, " +
            "column_comment as `comment`, " +
            "case when is_nullable='YES' then 0 else 1 end as 'not-null' " +
            "from information_schema.columns where table_schema=database() and table_name=#{table}";

    String TABLE_COMMENT_SQL = " select " +
            "table_comment as `comment` " +
            "from information_schema.tables where table_name=#{table}";

    String ALL_TABLE_SQL = "select table_name as `name` from information_schema.`TABLES` where table_schema=database()";

    public MysqlTableMetaParser(SqlExecutor sqlExecutor) {
        super(sqlExecutor);
        jdbcTypeMap.put("int", JDBCType.INTEGER);
        jdbcTypeMap.put("year", JDBCType.TIME);
        jdbcTypeMap.put("datetime", JDBCType.TIMESTAMP);

    }

    @Override
    Dialect getDialect() {
        return Dialect.MYSQL;
    }

    @Override
    String getTableMetaSql(String tname) {
        return TABLE_META_SQL;
    }

    @Override
    String getTableCommentSql(String tname) {
        return TABLE_COMMENT_SQL;
    }

    @Override
    String getAllTableSql() {
        return ALL_TABLE_SQL;
    }
}
