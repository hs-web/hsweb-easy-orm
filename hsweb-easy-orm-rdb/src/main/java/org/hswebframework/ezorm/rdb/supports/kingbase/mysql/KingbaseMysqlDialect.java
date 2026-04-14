package org.hswebframework.ezorm.rdb.supports.kingbase.mysql;

import org.hswebframework.ezorm.core.utils.StringUtils;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.LiteralDataType;
import org.hswebframework.ezorm.rdb.metadata.dialect.DefaultDialect;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;

import java.sql.Date;
import java.sql.JDBCType;

/**
 * KingbaseES MySQL 兼容模式方言.
 * <p>
 * KingbaseES MySQL 兼容版在 SQL 语法层面兼容 MySQL，但底层使用 PostgreSQL 协议通信。
 * 因此：
 * <ul>
 *   <li>数据类型映射：复用 MySQL 的类型映射</li>
 *   <li>标识符引用：使用反引号（MySQL 风格），KingbaseES MySQL 兼容模式不支持双引号</li>
 * </ul>
 *
 * @since 4.2
 */
public class KingbaseMysqlDialect extends DefaultDialect {

    public static final Dialect global = new KingbaseMysqlDialect();

    public KingbaseMysqlDialect() {
        super();
        // 复用 MySQL 的数据类型映射
        addDataTypeBuilder(JDBCType.CHAR, (meta) -> StringUtils.concat("char(", meta.getLength(), ")"));
        addDataTypeBuilder(JDBCType.VARCHAR, (meta) -> StringUtils.concat("varchar(", meta.getLength(), ")"));
        addDataTypeBuilder(JDBCType.NVARCHAR, (meta) -> StringUtils.concat("nvarchar(", meta.getLength(), ")"));

        addDataTypeBuilder(JDBCType.TIMESTAMP, (meta) -> "datetime(" + Math.min(6, meta.getLength()) + ")");
        addDataTypeBuilder(JDBCType.TIME, (meta) -> "time");
        addDataTypeBuilder(JDBCType.DATE, (meta) -> "date");
        addDataTypeBuilder(JDBCType.CLOB, (meta) -> "text");
        addDataTypeBuilder(JDBCType.LONGVARBINARY, (meta) -> "blob");
        addDataTypeBuilder(JDBCType.LONGVARCHAR, (meta) -> "longtext");
        addDataTypeBuilder(JDBCType.BLOB, (meta) -> "blob");
        addDataTypeBuilder(JDBCType.BIGINT, (meta) -> "bigint");
        addDataTypeBuilder(JDBCType.DOUBLE, (meta) -> "double");
        addDataTypeBuilder(JDBCType.INTEGER, (meta) -> "int");
        addDataTypeBuilder(JDBCType.NUMERIC, (meta) -> StringUtils.concat("decimal(", meta.getPrecision(32), ",", meta.getScale(), ")"));
        addDataTypeBuilder(JDBCType.DECIMAL, (meta) -> StringUtils.concat("decimal(", meta.getPrecision(32), ",", meta.getScale(), ")"));
        addDataTypeBuilder(JDBCType.TINYINT, (meta) -> "tinyint");
        addDataTypeBuilder(JDBCType.BOOLEAN, (meta) -> "tinyint");
        addDataTypeBuilder(JDBCType.BIGINT, (meta) -> "bigint");
        addDataTypeBuilder(JDBCType.OTHER, (meta) -> "other");
        addDataTypeBuilder(JDBCType.LONGNVARCHAR, (meta) -> "text");

        addDataTypeBuilder("int", (meta) -> "int");
        addDataTypeBuilder("json", meta -> "json");

        registerDataType("clob", DataType.builder(JdbcDataType.of(JDBCType.CLOB, String.class), c -> "text"));
        registerDataType("longnvarchar", DataType.builder(JdbcDataType.of(JDBCType.LONGNVARCHAR, String.class), c -> "longtext"));
        registerDataType("longvarchar", DataType.builder(JdbcDataType.of(JDBCType.LONGVARCHAR, String.class), c -> "longtext"));

        registerDataType("int", JdbcDataType.of(JDBCType.INTEGER, Integer.class));
        registerDataType("text", JdbcDataType.of(JDBCType.CLOB, String.class));
        registerDataType("longtext", JdbcDataType.of(JDBCType.LONGVARCHAR, String.class));
        registerDataType("year", JdbcDataType.of(JDBCType.DATE, Date.class));
        registerDataType("datetime", JdbcDataType.of(JDBCType.TIMESTAMP, Date.class));
    }

    @Override
    public DataType convertDataType(String dataType) {
        String rawType = dataType.trim();
        String type = rawType;
        if (type.contains("(")) {
            type = type.substring(0, type.indexOf("("));
        }
        type = normalizeType(type);
        if ("enum".equals(type) || "set".equals(type)) {
            return LiteralDataType.of(type, rawType, JDBCType.VARCHAR, String.class, false, false);
        }
        return super.convertDataType(rawType);
    }

    @Override
    public String getQuoteStart() {
        return "`";
    }

    @Override
    public String getQuoteEnd() {
        return "`";
    }

    @Override
    public boolean isColumnToUpperCase() {
        return false;
    }

    @Override
    public String getId() {
        return "kingbase-mysql";
    }

    @Override
    public String getName() {
        return "KingBase(MySQL)";
    }
}
