package org.hsweb.ezorm.rdb.meta.builder;

import java.sql.JDBCType;

public interface ColumnBuilder {
    ColumnBuilder name(String name);

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

    default ColumnBuilder varchar(int length) {
        return jdbcType(JDBCType.VARCHAR).length(length);
    }

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

    ColumnBuilder length(int len);

    ColumnBuilder length(int precision, int scale);

    TableBuilder commit();
}
