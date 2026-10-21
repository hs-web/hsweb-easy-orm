package org.hswebframework.ezorm.rdb.supports.kingbase.oracle;

import org.hswebframework.ezorm.rdb.mapping.defaults.record.Record;
import org.hswebframework.ezorm.rdb.metadata.LengthSupport;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.supports.commons.RDBTableMetadataParser;
import reactor.core.publisher.Flux;

import java.util.List;

public class KingbaseOracleTableMetadataParser extends RDBTableMetadataParser {

    private static final String TABLE_META_SQL = String.join(" ",
        "select cols.column_name as \"name\"",
        ", cols.data_type as \"data_type\"",
        ", cols.character_maximum_length as \"data_length\"",
        ", cols.numeric_precision as \"data_precision\"",
        ", cols.numeric_scale as \"data_scale\"",
        ", case when cols.is_nullable = 'YES' then 0 else 1 end as \"not_null\"",
        ", col_description(c.oid, a.attnum) as \"comment\"",
        ", cols.table_name as \"table_name\"",
        ", cols.data_type as \"column_type\"",
        "from information_schema.columns cols",
        "join pg_namespace n on n.nspname = cols.table_schema",
        "join pg_class c on c.relnamespace = n.oid and c.relname = cols.table_name",
        "join pg_attribute a on a.attrelid = c.oid and a.attname = cols.column_name",
        "where cols.table_schema = #{schema}",
        "and cols.table_name like #{table}");

    private static final String TABLE_COMMENT_SQL = String.join(" ",
        "select c.relname as \"table_name\"",
        ", obj_description(c.oid, 'pg_class') as \"comment\"",
        "from pg_class c",
        "join pg_namespace n on n.oid = c.relnamespace",
        "where n.nspname = #{schema}",
        "and c.relname like #{table}");

    private static final String ALL_TABLE_SQL =
        "select table_name as \"name\" from information_schema.tables where table_schema = #{schema}";

    private static final String TABLE_EXISTS_SQL =
        "select count(1) as \"total\" from information_schema.tables where table_schema = #{schema} and table_name = #{table}";

    public KingbaseOracleTableMetadataParser(RDBSchemaMetadata schema) {
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

    @Override
    protected void applyColumnInfo(RDBColumnMetadata column, Record record) {
        super.applyColumnInfo(column, record);
        if (column.getType() instanceof LengthSupport) {
            LengthSupport lengthSupport = (LengthSupport) column.getType();
            if (column.getLength() < 0) {
                column.setLength(lengthSupport.getLength());
            }
            if (column.getPrecision() < 0) {
                column.setPrecision(lengthSupport.getPrecision());
            }
            if (column.getScale() < 0) {
                column.setScale(lengthSupport.getScale());
            }
        }
    }
}
