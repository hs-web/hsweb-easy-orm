package org.hswebframework.ezorm.rdb.supports.oracle;

import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.meta.parser.RDBTableMetaParser;
import org.hswebframework.ezorm.rdb.dialect.Dialect;

public class OracleTableMetaParser extends RDBTableMetaParser {
    private final static String TABLE_META_SQL = "select distinct(cols.column_name) as \"name\"" +
            ",cols.table_name as \"table_name\"" +
            ",cols.data_type as \"data_type\"" +
            ",cols.data_length as \"data_length\"" +
            ",cols.data_precision as \"data_precision\"" +
            ",cols.data_scale as \"data_scale\"" +
            ",acc.comments as \"comment\"" +
            ",case when cols.nullable='Y' then 0 else 1 end as \"not-null\"" +
            ",cols.column_id from user_tab_columns cols " +
            "left join all_col_comments acc on acc.column_name=cols.column_name and acc.table_name=cols.table_name " +
            "where cols.table_name=upper(#{table}) " +
            "order by cols.column_id ";

    private final static String TABLE_COMMENT_SQL = "select comments as \"comment\" from user_tab_comments where table_type='TABLE' and table_name=upper(#{table})";

    private final static String ALL_TABLE_SQL = "select table_name as \"name\" from user_tab_comments where table_type='TABLE'";

    private static final String TABLE_EXISTS_SQL = "select count(1) as \"total\" from user_tab_comments where table_type='TABLE' and table_name=upper(#{table})";

    public OracleTableMetaParser(SyncSqlExecutor sqlExecutor) {
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
        return TABLE_META_SQL;
    }

    @Override
    protected Dialect getDialect() {
        return Dialect.ORACLE;
    }

    @Override
    protected String getTableCommentSql(String name) {
        return TABLE_COMMENT_SQL;
    }

    @Override
    protected String getAllTableSql() {
        return ALL_TABLE_SQL;
    }

    @Override
    public String getTableExistsSql() {
        return TABLE_EXISTS_SQL;
    }
}
