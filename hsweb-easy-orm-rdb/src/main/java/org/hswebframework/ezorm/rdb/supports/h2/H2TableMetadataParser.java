package org.hswebframework.ezorm.rdb.supports.h2;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.supports.commons.RDBTableMetadataParser;
import reactor.core.publisher.Flux;

import java.util.List;


public class H2TableMetadataParser extends RDBTableMetadataParser {
    private static final String TABLE_META_SQL =
            String.join(" ",
                    "SELECT",
                    "column_name AS \"name\",",
                    "data_type AS \"data_type\",",
                    "table_name AS \"table_name\",",
                    "character_maximum_length as \"data_length\",",
                    "numeric_precision as \"data_precision\",",
                    "numeric_scale as \"data_scale\",",
                    "case when is_nullable='YES' then 0 else 1 end as \"not_null\",",
                    "remarks as \"comment\" ",
                    "FROM information_schema.columns ",
                    "WHERE table_name like upper(#{table}) and table_schema=#{schema}");

    private static final String TABLE_COMMENT_SQL =
            String.join(" ",
                    "SELECT",
                    "table_name as \"table_name\" ,",
                    "remarks as \"comment\" ",
                    "FROM information_schema.tables WHERE table_type='TABLE' and table_name like upper(#{table}) and table_schema=#{schema}");

    private static final String ALL_TABLE_SQL =
            "SELECT table_name as \"name\" " +
                    "FROM information_schema.tables where table_type='TABLE' and table_schema=#{schema}";

    private static final String TABLE_EXISTS_SQL = "SELECT count(1) as \"total\" FROM information_schema.columns " +
            "WHERE table_name = upper(#{table}) and table_schema=#{schema}";

    public H2TableMetadataParser(RDBSchemaMetadata schema) {
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

    @Override
    public Flux<RDBTableMetadata> parseAllReactive() {
        return super.fastParseAllReactive();
    }
}
