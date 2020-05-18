package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.supports.commons.RDBTableMetadataParser;

import java.util.List;

public class PostgresqlTableMetadataParser extends RDBTableMetadataParser {
    private static final String TABLE_META_SQL =
            String.join(" ",
                    "select column_name::varchar as \"name\"",
                    ", udt_name::varchar as \"data_type\"",
                    ", character_maximum_length::int4 as \"data_length\"",
                    ", numeric_precision::int4 as \"data_precision\"",
                    ", numeric_scale::int4 as \"data_scale\"",
                    ", case when is_nullable = 'YES' then 0 else 1 end as \"not_null\"",
                    ",col_description(a.attrelid,a.attnum) as \"comment\"",
                    ",table_name as \"table_name\"",
                    "from information_schema.columns columns ,",
                    "pg_class as c,pg_attribute as a",
                    "where a.attrelid = c.oid and a.attnum>0 and a.attname = columns.column_name and c.relname=columns.table_name",
                    "and table_schema = #{schema}",
                    "and table_name like #{table}"
            );

    private static final String TABLE_COMMENT_SQL = String.join(" ",
            "select"
            , "relname as \"table_name\""
            , "cast(obj_description(relfilenode,'pg_class') as varchar) as \"comment\" "
            , "from pg_class c",
            "where relname like #{table} and relkind = 'r' and relname not like 'pg_%'",
            "and relname not like 'sql_%'"
    );

    private static final String ALL_TABLE_SQL = "select table_name::varchar as \"name\" from information_schema.TABLES where table_schema=#{schema}";

    private static final String TABLE_EXISTS_SQL = "select count(1) as total from information_schema.TABLES where table_schema=#{schema} and table_name like #{table}";

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
        return TABLE_EXISTS_SQL;
    }

    @Override
    public List<RDBTableMetadata> parseAll() {
        return super.fastParseAll();
    }
}
