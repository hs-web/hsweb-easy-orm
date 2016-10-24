package org.hsweb.ezorm.meta.builder;

import java.sql.JDBCType;

public interface ColumnBuilder {
    ColumnBuilder name(String name);

    ColumnBuilder dataType(String dataType);

    ColumnBuilder jdbcType(JDBCType jdbcType);

    ColumnBuilder javaType(Class javaType);

    ColumnBuilder comment(String comment);

    ColumnBuilder notNull();

    ColumnBuilder primaryKey();

    ColumnBuilder index();

    ColumnBuilder length(int len);

    ColumnBuilder length(int precision, int scale);

    TableBuilder commit();
}
