package org.hswebframework.ezorm.rdb.meta.parser;

import org.hswebframework.ezorm.rdb.executor.SqlExecutor;
import org.hswebframework.ezorm.rdb.render.dialect.Dialect;

import java.sql.JDBCType;

public class PGSqlTableMetaParser extends AbstractTableMetaParser {
    static final String TABLE_META_SQL = "select column_name as \"name\"" +
            " , udt_name as \"data_type\"" +
            " , character_maximum_length as \"data_length\"" +
            " , numeric_precision as \"data_precision\"" +
            " , numeric_scale as \"data_scale\"" +
            " , case when is_nullable = 'YES' then 0 else 1 end as \"not-null\"" +
            " ,col_description(a.attrelid,a.attnum) as \"comment\"" +
            " from information_schema.columns columns ," +
            "     pg_class as c,pg_attribute as a" +
            " where a.attrelid = c.oid and a.attnum>0 and a.attname = columns.column_name and c.relname=columns.table_name" +
            " and table_schema = %s" +
            " and table_name = #{table}";

    static final String TABLE_COMMENT_SQL = "select cast(obj_description(relfilenode,'pg_class') as varchar)" +
            "  as \"comment\" from pg_class c" +
            " where relname=#{table} and relkind = 'r' and relname not like 'pg_%'" +
            " and relname not like 'sql_%'";

    static final String ALL_TABLE_SQL = "select table_name as \"name\" from information_schema.TABLES where table_schema=%s";

    static final String TABLE_EXISTS_SQL = "select count(1) as total from information_schema.TABLES where table_schema=%s and table_name=#{table}";

    public PGSqlTableMetaParser(SqlExecutor sqlExecutor) {
        super(sqlExecutor);
    }

    protected String getRealDatabaseName() {
        String db = getDatabaseName();
        if (db == null) {
            return "current_schema()";
        }
        return "'" + db + "'";
    }

    @Override
    Dialect getDialect() {
        return Dialect.POSTGRES;
    }

    @Override
    String getTableMetaSql(String tname) {
        return String.format(TABLE_META_SQL, getRealDatabaseName());
    }

    @Override
    String getTableCommentSql(String tname) {
        return TABLE_COMMENT_SQL;
    }

    @Override
    String getAllTableSql() {
        return String.format(ALL_TABLE_SQL, getRealDatabaseName());
    }

    @Override
    String getTableExistsSql() {
        return String.format(TABLE_EXISTS_SQL, getRealDatabaseName());
    }
}
