package org.hswebframework.ezorm.rdb.supports.kingbaseES.mysql;

import org.hswebframework.ezorm.rdb.mapping.defaults.record.Record;
import org.hswebframework.ezorm.rdb.metadata.LengthSupport;
import org.hswebframework.ezorm.rdb.metadata.LiteralDataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlTableMetadataParser;

import java.sql.JDBCType;
import java.util.Locale;

/**
 * Applies MySQL type mappings to Kingbase metadata and completes omitted lengths.
 */
public class KingbaseMysqlTableMetadataParser extends MysqlTableMetadataParser {

    private static final String TABLE_META_SQL = String.join(" ",
        "select c.column_name as `name`, c.data_type as `data_type`,",
        "c.character_maximum_length as `data_length`, c.numeric_precision as `data_precision`,",
        "c.numeric_scale as `data_scale`, c.column_comment as `comment`,",
        "c.table_name as `table_name`, c.column_type as `column_type`, t.typtype as `type_kind`,",
        "case when c.is_nullable='YES' then 0 else 1 end as 'not_null'",
        "from information_schema.columns c",
        "left join pg_namespace n on n.nspname = c.table_schema",
        "left join pg_class r on r.relnamespace = n.oid and r.relname = c.table_name",
        "left join pg_attribute a on a.attrelid = r.oid and a.attname = c.column_name and a.attnum > 0",
        "left join pg_type t on t.oid = a.atttypid",
        "where c.table_schema=#{schema} and c.table_name like #{table}");

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
    protected String getTableMetaSql(String name) {
        return TABLE_META_SQL;
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
        String columnType = record.getString("column_type")
            .orElseGet(() -> record.getString("data_type").orElse(null));
        if (columnType != null) {
            record.putValue("column_type", mysqlTypeName(columnType, record));
        }
        super.applyColumnInfo(column, record);
        String kind = record.getString("type_kind").orElse("");
        if (columnType != null && ("e".equals(kind) || "l".equals(kind) || "y".equals(kind))) {
            // Use the catalog kind, not a generated name prefix, to recognize ENUM/SET.
            String type = "y".equals(kind) ? "set" : "enum";
            column.setType(LiteralDataType.of(type, columnType, JDBCType.VARCHAR, String.class, false, false));
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

    private String mysqlTypeName(String type, Record record) {
        String name = type.trim().toLowerCase(Locale.ROOT);
        int arguments = name.indexOf('(');
        String suffix = arguments < 0 ? "" : name.substring(arguments);
        String base = arguments < 0 ? name : name.substring(0, arguments).trim();
        switch (base) {
            case "character varying":
                return "varchar" + suffix;
            case "character":
            case "bpchar":
                return "char" + suffix;
            case "integer":
                return "int" + suffix;
            case "numeric":
                return "decimal" + suffix;
            case "float4":
            case "float8":
            case "double precision":
                // KES omits FLOAT/DOUBLE parameters from column_type but reports precision/scale.
                if (suffix.isEmpty() && record.get("data_precision").isPresent()
                    && record.get("data_scale").isPresent()) {
                    suffix = "(" + record.getInteger("data_precision").get()
                        + "," + record.getInteger("data_scale").get() + ")";
                }
                return ("float4".equals(base) ? "float" : "double") + suffix;
            case "boolean":
            case "bool":
                return "tinyint(1)";
            default:
                return type;
        }
    }
}
