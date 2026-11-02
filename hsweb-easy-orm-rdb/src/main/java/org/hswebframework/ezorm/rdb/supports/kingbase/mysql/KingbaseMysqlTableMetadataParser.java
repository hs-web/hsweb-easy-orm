package org.hswebframework.ezorm.rdb.supports.kingbase.mysql;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.supports.commons.RDBTableMetadataParser;
import reactor.core.publisher.Flux;

import java.util.List;

/**
 * KingbaseES MySQL 兼容模式的表元数据解析器.
 * <p>
 * 通过 r2dbc-postgresql 驱动查询 information_schema 时，
 * {@code character_maximum_length}、{@code numeric_precision}、{@code numeric_scale}
 * 等字段的返回类型为 varchar（而非 int），需要在 SQL 中进行显式 CAST 转换，
 * 避免 {@code ClassCastException: Cannot cast String to Number}。
 *
 * @since 4.2
 */
public class KingbaseMysqlTableMetadataParser extends RDBTableMetadataParser {

    private static final String TABLE_META_SQL = String.join(" ",
            "select",
            "column_name as `name`,",
            "data_type as `data_type`,",
            "cast(character_maximum_length as integer) as `data_length`,",
            "cast(numeric_precision as integer) as `data_precision`,",
            "cast(numeric_scale as integer) as `data_scale`,",
            "column_comment as `comment`,",
            "table_name as `table_name`,",
            "column_type as `column_type`,",
            "case when is_nullable='YES' then 0 else 1 end as `not_null`",
            "from information_schema.columns where table_schema=#{schema} and table_name like #{table}");

    private static final String TABLE_COMMENT_SQL = String.join(" ",
            "select ",
            "table_comment as `comment`",
            ",table_name as `table_name`",
            "from information_schema.tables where table_schema=#{schema} and table_name like #{table}");

    private static final String ALL_TABLE_SQL =
            "select table_name as `name` from information_schema.`TABLES` where table_schema=#{schema}";

    private static final String TABLE_EXISTS_SQL =
            "select count(1) as `total` from information_schema.`TABLES` where table_schema=#{schema} and table_name=#{table}";

    public KingbaseMysqlTableMetadataParser(RDBSchemaMetadata schema) {
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
