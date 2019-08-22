package org.hswebframework.ezorm.rdb.codec;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.core.ValueCodec;

import java.sql.JDBCType;

@AllArgsConstructor
public class BooleanValueCodec implements ValueCodec {

    private JDBCType jdbcType;

    @Override
    public Object encode(Object value) {
        if (jdbcType == JDBCType.BOOLEAN || value == null) {
            return value;
        }
        if (Boolean.TRUE.equals(value)) {
            return 1;
        }
        if (Boolean.FALSE.equals(value)) {
            return 0;
        }
        return value;
    }

    @Override
    public Object decode(Object data) {
        if (null == data) {
            return false;
        }
        if (data instanceof Boolean) {
            return data;
        }
        return "1".equals(String.valueOf(data)) || "true".equals(String.valueOf(data));
    }
}
