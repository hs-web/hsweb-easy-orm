package org.hswebframework.ezorm.rdb.supports.mysql;

import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.meta.parser.RDBTableMetaParser;
import org.hswebframework.ezorm.rdb.dialect.Dialect;

public class MysqlTableMetaParser extends RDBTableMetaParser {
    private static final String TABLE_META_SQL = " select " +
            "column_name as `name`, " +
            "data_type as `data_type`, " +
            "character_maximum_length as `data_length`, " +
            "numeric_precision as `data_precision`, " +
            "numeric_scale as `data_scale`, " +
            "column_comment as `comment`, " +
            "case when is_nullable='YES' then 0 else 1 end as 'not-null' " +
            "from information_schema.columns where table_schema=%s and table_name=#{table}";

    private static final String TABLE_COMMENT_SQL = " select " +
            "table_comment as `comment` " +
            "from information_schema.tables where table_schema=%s and table_name=#{table}";

    private static final String ALL_TABLE_SQL = "select table_name as `name` from information_schema.`TABLES` where table_schema=%s";

    private static final String TABLE_EXISTS_SQL = "select count(1) as 'total' from information_schema.`TABLES` where table_schema=%s and table_name=#{table}";

    public MysqlTableMetaParser(SyncSqlExecutor sqlExecutor) {
        super(sqlExecutor);
    }

    private String getRealDatabaseName() {
        String db = getSchemaName();
        if (db == null) {
            return "database()";
        }
        return "'" + db + "'";
    }

    @Override
    protected String getTableMetaSql(String name) {
        return String.format(TABLE_META_SQL, getRealDatabaseName());
    }

    @Override
    protected Dialect getDialect() {
        return Dialect.MYSQL;
    }

    @Override
    protected String getTableCommentSql(String name) {
        return String.format(TABLE_COMMENT_SQL, getRealDatabaseName());
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
