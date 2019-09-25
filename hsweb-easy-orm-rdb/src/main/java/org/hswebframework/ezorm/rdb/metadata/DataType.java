package org.hswebframework.ezorm.rdb.metadata;

import java.sql.JDBCType;

/**
 * @see JdbcDataType
 * @see CustomDataType
 */
public interface DataType {

    String getId();

    String getName();

    JDBCType getJdbcType();

    Class getJavaType();
}
