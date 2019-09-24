package org.hswebframework.ezorm.rdb.supports.posgres;

import org.hswebframework.ezorm.rdb.metadata.DataType;

import java.sql.JDBCType;

public class JsonType implements DataType {
    @Override
    public String getId() {
        return "json";
    }

    @Override
    public String getName() {
        return "json";
    }

    @Override
    public JDBCType getJdbcType() {
        return JDBCType.CLOB;
    }
}
