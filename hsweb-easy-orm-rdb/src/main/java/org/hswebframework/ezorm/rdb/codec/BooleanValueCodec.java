package org.hswebframework.ezorm.rdb.codec;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.core.ValueCodec;

import java.sql.JDBCType;
import java.sql.SQLType;

@AllArgsConstructor
public class BooleanValueCodec implements ValueCodec<Object, Boolean> {

    private SQLType sqlType;

    @Override
    public Object encode(Object value) {
        if (value == null) {
            return null;
        }
        if (!(value instanceof Boolean)) {
            value = "1".equals(String.valueOf(value)) || "true".equals(String.valueOf(value));
        }

        if (Boolean.TRUE.equals(value)) {
            return sqlType == JDBCType.BOOLEAN ? true : 1;
        }
        if (Boolean.FALSE.equals(value)) {
            return sqlType == JDBCType.BOOLEAN ? false : 0;
        }
        return value;
    }

    @Override
    public Boolean decode(Object data) {
        if (null == data) {
            return false;
        }
        if (data instanceof Boolean) {
            return (Boolean) data;
        }
        return "1".equals(String.valueOf(data)) || "true".equals(String.valueOf(data));
    }
}
