package org.hswebframework.ezorm.rdb.supports.posgres;

import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.supports.commons.RDBTableMetaParser;
import org.hswebframework.ezorm.rdb.dialect.Dialect;

public class PostgreSQLTableMetaParser extends RDBTableMetaParser {
    private static final String TABLE_META_SQL = "select column_name as \"name\"" +
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

    private static final String TABLE_COMMENT_SQL = "select cast(obj_description(relfilenode,'pg_class') as varchar)" +
            "  as \"comment\" from pg_class c" +
            " where relname=#{table} and relkind = 'r' and relname not like 'pg_%'" +
            " and relname not like 'sql_%'";

    private static final String ALL_TABLE_SQL = "select table_name as \"name\" from information_schema.TABLES where table_schema=%s";

    private static final String TABLE_EXISTS_SQL = "select count(1) as total from information_schema.TABLES where table_schema=%s and table_name=#{table}";

    public PostgreSQLTableMetaParser(SyncSqlExecutor sqlExecutor) {
        super(sqlExecutor);
    }

    private String getRealDatabaseName() {
        String db = getSchemaName();
        if (db == null) {
            return "current_schema()";
        }
        return "'" + db + "'";
    }

    @Override
    protected String getTableMetaSql(String name) {
        return String.format(TABLE_META_SQL, getRealDatabaseName());
    }

    @Override
    protected Dialect getDialect() {
        return Dialect.POSTGRES;
    }

    @Override
    protected String getTableCommentSql(String name) {
        return TABLE_COMMENT_SQL;
    }

    @Override
    protected String getAllTableSql() {
        return String.format(ALL_TABLE_SQL, getRealDatabaseName());
    }

    @Override
    public String getTableExistsSql() {
        return String.format(TABLE_EXISTS_SQL, getRealDatabaseName());
    }
}
