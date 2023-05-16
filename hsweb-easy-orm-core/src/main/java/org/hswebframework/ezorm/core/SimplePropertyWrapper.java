package org.hswebframework.ezorm.core;

import com.alibaba.fastjson.JSON;
import org.hswebframework.ezorm.core.utils.StringUtils;
import org.hswebframework.utils.ClassUtils;
import org.hswebframework.utils.DateTimeUtils;
import org.hswebframework.utils.time.DateFormatter;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * @since 1.0
 */
public class SimplePropertyWrapper implements PropertyWrapper {

    private final Object value;

    public SimplePropertyWrapper(Object value) {
        this.value = value;
    }

    @Override
    public <T> T getValue() {
        return (T) value;
    }

    @Override
    public int toInt() {
        if (value instanceof Number) {
            return ((Number) value).intValue();
        }
        return Integer.parseInt(String.valueOf(value));
    }

    @Override
    public double toDouble() {
        if (value instanceof Number) {
            return ((Number) value).doubleValue();
        }
        return Double.parseDouble(String.valueOf(value));
    }

    @Override
    public boolean isTrue() {
        if (value instanceof Boolean) {
            return ((Boolean) value);
        }

        return Objects.equals(1, value) ||
                Objects.equals("true", value) ||
                Objects.equals("y", value) ||
                Objects.equals("yes", value)||
                Objects.equals("1", value);
    }

    @Override
    public Date toDate() {
        if (value instanceof Date) return ((Date) value);
        return DateFormatter.fromString(toString());
    }

    @Override
    public Date toDate(String format) {
        if (value instanceof Date) return ((Date) value);
        return DateTimeUtils.formatDateString(toString(), format);
    }

    @Override
    public <T> T toBean(Class<T> type) {
        if (valueTypeOf(type)) return ((T) getValue());
        return JSON.parseObject(toString(), type);
    }

    @Override
    public List<Map> toList() {
        return toBeanList(Map.class);
    }

    @Override
    public Map<String, Object> toMap() {
        return toBean(Map.class);
    }

    @Override
    public <T> List<T> toBeanList(Class<T> type) {
        if (getValue() instanceof List) return ((List) getValue());
        return JSON.parseArray(toString(), type);
    }

    @Override
    public boolean isNullOrEmpty() {
        return StringUtils.isNullOrEmpty(value);
    }

    @Override
    public boolean valueTypeOf(Class<?> type) {
        if (value == null) return false;
        return ClassUtils.instanceOf(value.getClass(), type);
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

}
