package org.hswebframework.ezorm.rdb.supports.posgres;

import org.hswebframework.ezorm.rdb.metadata.DataType;

import java.sql.JDBCType;

public class JsonbType implements DataType {
    @Override
    public String getId() {
        return "jsonb";
    }

    @Override
    public String getName() {
        return "jsonb";
    }

    @Override
    public JDBCType getJdbcType() {
        return JDBCType.CLOB;
    }
}
