package org.hsweb.ezorm.rdb.meta.parser;

import org.hsweb.ezorm.rdb.executor.SqlExecutor;
import org.hsweb.ezorm.rdb.render.dialect.Dialect;

import java.sql.JDBCType;

public class OracleTableMetaParser extends AbstractTableMetaParser {

    final static String TABLE_META_SQL = "select distinct(cols.column_name) as \"name\"" +
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

    final static String TABLE_COMMENT_SQL = "select comments as \"comment\" from user_tab_comments where table_type='TABLE' and table_name=upper(#{table})";

    final static String ALL_TABLE_SQL = "select table_name as \"name\" from user_tab_comments where table_type='TABLE'";

    static final String TABLE_EXISTS_SQL = "select count(1) as \"total\" from user_tab_comments where table_type='TABLE' and table_name=upper(#{table})";


    @Override
    Dialect getDialect() {
        return Dialect.ORACLE;
    }

    public OracleTableMetaParser(SqlExecutor sqlExecutor) {
        super(sqlExecutor);
        jdbcTypeMap.put("varchar2", JDBCType.VARCHAR);
        jdbcTypeMap.put("number", JDBCType.NUMERIC);
        jdbcTypeMap.put("date", JDBCType.TIMESTAMP);
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

    @Override
    public String getTableExistsSql() {
        return TABLE_EXISTS_SQL;
    }
}
