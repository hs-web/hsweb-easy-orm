package org.hswebframework.ezorm.rdb.operator.ddl;

import org.hswebframework.ezorm.core.DefaultValue;
import org.hswebframework.ezorm.rdb.metadata.NativeSqlDefaultValue;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;

import java.sql.JDBCType;
import java.util.function.Consumer;

public interface ColumnBuilder {
    ColumnBuilder custom(Consumer<RDBColumnMetadata> consumer);

    ColumnBuilder name(String name);

    ColumnBuilder alias(String name);

    ColumnBuilder dataType(String dataType);

    ColumnBuilder jdbcType(JDBCType jdbcType);

    default ColumnBuilder jdbcType(String jdbcType) {
        JDBCType.valueOf(jdbcType.toUpperCase());
        return this;
    }

    ColumnBuilder javaType(Class javaType);

    ColumnBuilder comment(String comment);

    ColumnBuilder notNull();

    ColumnBuilder primaryKey();

    ColumnBuilder columnDef(String def);

    default ColumnBuilder varchar(int length) {
        return jdbcType(JDBCType.VARCHAR).length(length);
    }

    default ColumnBuilder defaultValue(String defaultSql) {
        return defaultValue(NativeSqlDefaultValue.of(defaultSql));
    }

    ColumnBuilder defaultValue(DefaultValue value);

    default ColumnBuilder number(int precision, int scale) {
        return jdbcType(JDBCType.NUMERIC).length(precision, scale);
    }

    default ColumnBuilder number(int len) {
        return jdbcType(JDBCType.NUMERIC).length(len, 0);
    }

    default ColumnBuilder clob() {
        return jdbcType(JDBCType.CLOB);
    }

    default ColumnBuilder integer() {
        return jdbcType(JDBCType.INTEGER);
    }

    default ColumnBuilder datetime() {
        return jdbcType(JDBCType.TIMESTAMP);
    }

    default ColumnBuilder tinyint() {
        return jdbcType(JDBCType.TINYINT);
    }

    ColumnBuilder property(String propertyName, Object value);

    ColumnBuilder length(int len);

    ColumnBuilder length(int precision, int scale);

    TableBuilder commit();
}
