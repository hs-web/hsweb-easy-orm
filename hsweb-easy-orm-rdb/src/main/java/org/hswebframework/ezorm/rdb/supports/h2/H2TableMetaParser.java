package org.hswebframework.ezorm.rdb.supports.h2;

import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.supports.commons.RDBTableMetaParser;
import org.hswebframework.ezorm.rdb.dialect.Dialect;

public class H2TableMetaParser extends RDBTableMetaParser {
    private static final String TABLE_META_SQL =
            "SELECT column_name AS \"name\"," +
                    "type_name AS \"data_type\"," +
                    "character_maximum_length as \"data_length\"," +
                    "numeric_precision as \"data_precision\"," +
                    "numeric_scale as \"data_scale\"," +
                    "case when is_nullable='YES' then 0 else 1 end as \"not-null\"," +
                    "remarks as \"comment\" " +
                    "FROM information_schema.columns " +
                    "WHERE table_name = upper(#{table}) and table_schema=%s";
    private static final String TABLE_COMMENT_SQL =
            "SELECT remarks as \"comment\" " +
                    "FROM information_schema.tables WHERE table_type='TABLE' and table_name=upper(#{table}) and table_schema=%s";
    private static final String ALL_TABLE_SQL =
            "select table_name as \"name\" " +
                    "FROM information_schema.tables where table_type='TABLE'  and table_name=upper(#{table}) and table_schema=%s";

    private static final String TABLE_EXISTS_SQL = "SELECT count(1) as \"total\" FROM information_schema.columns " +
            "WHERE table_name = upper(#{table}) and table_schema=%s";

    public H2TableMetaParser(SyncSqlExecutor sqlExecutor) {
        super(sqlExecutor);
    }

    private String getRealDatabaseName() {
        String db = getSchemaName();
        if (db == null) {
            return "schema()";
        }
        return "'" + db + "'";
    }

    @Override
    protected String getTableMetaSql(String name) {
        return String.format(TABLE_META_SQL, getRealDatabaseName());
    }

    @Override
    protected Dialect getDialect() {
        return Dialect.H2;
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
