package org.hswebframework.ezorm.rdb.supports.kingbase.mysql;

import org.hswebframework.ezorm.rdb.mapping.defaults.record.Record;
import org.hswebframework.ezorm.rdb.metadata.LengthSupport;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlTableMetadataParser;

import java.util.Locale;

/**
 * Completes length and precision values omitted by Kingbase metadata rows.
 */
public class KingbaseMysqlTableMetadataParser extends MysqlTableMetadataParser {

    private static final String TABLE_COMMENT_SQL = String.join(" ",
        "select c.relname as \"table_name\"",
        ", obj_description(c.oid, 'pg_class') as \"comment\"",
        "from pg_class c",
        "join pg_namespace n on n.oid = c.relnamespace",
        "where n.nspname = #{schema}",
        "and c.relname like #{table}");

    public KingbaseMysqlTableMetadataParser(RDBSchemaMetadata schema) {
        super(schema);
    }

    @Override
    protected String getTableCommentSql(String name) {
        return TABLE_COMMENT_SQL;
    }

    @Override
    protected void applyColumnInfo(RDBColumnMetadata column, Record record) {
        // Kingbase 元数据视图中数值列可能以字符串返回，统一规整为数值
        for (String key : new String[]{"data_length", "data_precision", "data_scale"}) {
            record.get(key).ifPresent(value -> {
                if (value instanceof String) {
                    record.putValue(key, Integer.valueOf((String) value));
                }
            });
        }
        super.applyColumnInfo(column, record);
        if (column.getName() != null) {
            column.setAlias(column.getName().toLowerCase(Locale.ROOT));
        }
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
