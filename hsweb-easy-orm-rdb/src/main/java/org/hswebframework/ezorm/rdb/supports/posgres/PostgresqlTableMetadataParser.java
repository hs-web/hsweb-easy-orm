package org.hswebframework.ezorm.rdb.supports.posgres;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.commons.RDBTableMetadataParser;

public class PostgresqlTableMetadataParser extends RDBTableMetadataParser {
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
            " and table_schema = #{schema}" +
            " and table_name = #{table}";

    private static final String TABLE_COMMENT_SQL = "select cast(obj_description(relfilenode,'pg_class') as varchar)" +
            "  as \"comment\" from pg_class c" +
            " where relname=#{table} and relkind = 'r' and relname not like 'pg_%'" +
            " and relname not like 'sql_%'";

    private static final String ALL_TABLE_SQL = "select table_name as \"name\" from information_schema.TABLES where table_schema=#{schema}";

    private static final String TABLE_EXISTS_SQL = "select count(1) as total from information_schema.TABLES where table_schema=#{schema} and table_name=#{table}";

    public PostgresqlTableMetadataParser(RDBSchemaMetadata schema) {
        super(schema);
    }

    @Override
    protected String getTableMetaSql(String name) {
        return TABLE_META_SQL;
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
        return  TABLE_EXISTS_SQL;
    }
}
