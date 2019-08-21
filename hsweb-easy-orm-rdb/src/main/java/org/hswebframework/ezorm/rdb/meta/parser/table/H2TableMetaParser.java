package org.hswebframework.ezorm.rdb.meta.parser.table;

import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.render.dialect.Dialect;

public class H2TableMetaParser extends RDBTableMetaParser {
    static final String TABLE_META_SQL =
            "SELECT column_name AS \"name\"," +
                    "type_name AS \"data_type\"," +
                    "character_maximum_length as \"data_length\"," +
                    "numeric_precision as \"data_precision\"," +
                    "numeric_scale as \"data_scale\"," +
                    "case when is_nullable='YES' then 0 else 1 end as \"not-null\"," +
                    "remarks as \"comment\" " +
                    "FROM information_schema.columns " +
                    "WHERE TABLE_NAME = upper(#{table}) and TABLE_SCHEMA=%s";
    static final String TABLE_COMMENT_SQL =
            "SELECT remarks as \"comment\" " +
                    "FROM information_schema.tables WHERE table_type='TABLE' and table_name=upper(#{table})";
    static final String ALL_TABLE_SQL =
            "select table_name as \"name\" " +
                    "FROM information_schema.tables where table_type='TABLE'  and table_name=upper(#{table})";

    static final String TABLE_EXISTS_SQL = "SELECT count(1) as \"total\" FROM information_schema.columns WHERE TABLE_NAME = upper(#{table})  and table_name=upper(#{table})";

    public H2TableMetaParser(SyncSqlExecutor sqlExecutor) {
        super(sqlExecutor);
    }

    protected String getRealDatabaseName() {
        String db = getSchemaName();
        if (db == null) {
            return "schema()";
        }
        return "'" + db + "'";
    }

    @Override
    String getTableMetaSql(String name) {
        return String.format(TABLE_META_SQL, getRealDatabaseName());
    }

    @Override
    protected Dialect getDialect() {
        return Dialect.H2;
    }

    @Override
    String getTableCommentSql(String name) {
        return TABLE_COMMENT_SQL;
    }

    @Override
    String getAllTableSql() {
        return ALL_TABLE_SQL;
    }

    @Override
    public String getTableExistsSql() {
        return TABLE_EXISTS_SQL;
    }
}
