package org.hswebframework.ezorm.rdb.metadata;

import java.sql.JDBCType;

public interface DataType   {

    String getId();

    String getName();

    JDBCType getJdbcType();
}
