package org.hswebframework.ezorm.rdb.supports.oracle;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.supports.commons.RDBTableMetadataParser;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import reactor.core.publisher.Flux;

import java.util.List;

public class OracleTableMetadataParser extends RDBTableMetadataParser {
    private final static String TABLE_META_SQL = String.join(" ",
            "select distinct(cols.column_name) as \"name\"",
            ",cols.table_name as \"table_name\"",
            ",cols.data_type as \"data_type\"",
            ",cols.data_length as \"data_length\"",
            ",cols.data_precision as \"data_precision\"",
            ",cols.data_scale as \"data_scale\"",
            ",acc.comments as \"comment\"",
            ",case when cols.nullable='Y' then 0 else 1 end as \"not_null\"",
            ",cols.table_name as \"table_name\"",
            ",cols.column_id from all_tab_cols cols ",
            "left join all_col_comments acc on acc.OWNER = #{schema} and acc.column_name=cols.column_name and acc.table_name=cols.table_name ",
            "where cols.owner=#{schema} and cols.table_name like upper(#{table}) and cols.virtual_column='NO' ",
            "order by cols.column_id ");

    private final static String TABLE_COMMENT_SQL = String.join(" ",
            "select",
            "table_name as \"table_name\",",
            "comments as \"comment\"",
            "from all_tab_comments ",
            "where owner=#{schema} and table_type='TABLE' and table_name like upper(#{table})");

    private final static String ALL_TABLE_SQL = "select table_name as \"name\" from all_tab_comments where owner=#{schema} and table_type='TABLE'";

    private static final String TABLE_EXISTS_SQL = "select count(1) as \"total\" from all_tab_comments where owner=#{schema} and table_type='TABLE' and table_name=upper(#{table})";

    public OracleTableMetadataParser(RDBSchemaMetadata schema) {
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
