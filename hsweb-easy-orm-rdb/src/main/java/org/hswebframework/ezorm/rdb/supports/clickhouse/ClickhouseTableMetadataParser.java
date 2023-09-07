package org.hswebframework.ezorm.rdb.supports.clickhouse;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.supports.commons.RDBTableMetadataParser;
import reactor.core.publisher.Flux;

import java.util.List;

/**
 * @className Clickhouse
 * @Description TODO
 * @Author dengpengyu
 * @Date 2023/9/4 14:56
 * @Vesion 1.0
 */
public class ClickhouseTableMetadataParser extends RDBTableMetadataParser {
    private static final String TABLE_META_SQL = String.join(" ",
            "select",
            "column_name as `name`,",
            "data_type as `data_type`,",
            "character_maximum_length as `data_length`,",
            "numeric_precision as `data_precision`,",
            "numeric_scale as `data_scale`,",
            "column_comment as `comment`,",
            "table_name as `table_name`,",
            "case when is_nullable=0 then 0 else 1 end ",
            "from information_schema.columns where table_schema=#{schema} and table_name like #{table}");

    private static final String TABLE_COMMENT_SQL = String.join(" ",
            "select ",
            "`comment`",
            ",name as `table_name`",
            "from system.tables where database=#{schema} and name like #{table}");


    private static final String ALL_TABLE_SQL = "select table_name as `name` from system.tables where database=#{schema}";

    private static final String TABLE_EXISTS_SQL = "select count() as `total` from system.tables where database=#{schema} and name=#{table}";

    public ClickhouseTableMetadataParser(RDBSchemaMetadata schema) {
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
