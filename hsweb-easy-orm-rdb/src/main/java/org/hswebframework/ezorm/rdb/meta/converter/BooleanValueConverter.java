package org.hswebframework.ezorm.rdb.meta.converter;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.core.ValueConverter;

import java.sql.JDBCType;

@AllArgsConstructor
public class BooleanValueConverter implements ValueConverter {

    private JDBCType jdbcType;

    @Override
    public Object getData(Object value) {
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
    public Object getValue(Object data) {
        if (null == data) return false;
        if (data instanceof Boolean) return data;
        return "1".equals(String.valueOf(data)) || "true".equals(String.valueOf(data));
    }
}
