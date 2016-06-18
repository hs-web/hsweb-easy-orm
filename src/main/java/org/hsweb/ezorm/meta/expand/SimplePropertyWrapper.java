package org.hsweb.ezorm.meta.expand;

import com.alibaba.fastjson.JSON;
import org.hsweb.commons.ClassUtils;
import org.hsweb.commons.DateTimeUtils;
import org.hsweb.commons.StringUtils;

import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * Created by zhouhao on 16-6-4.
 */
public class SimplePropertyWrapper implements PropertyWrapper {

    private Object value;

    public SimplePropertyWrapper(Object value) {
        this.value = value;
    }

    @Override
    public <T> T getValue() {
        return (T) value;
    }

    @Override
    public int toInt() {
        return StringUtils.toInt(value);
    }

    @Override
    public double toDouble() {
        return StringUtils.toDouble(value);
    }

    @Override
    public boolean isTrue() {
        return StringUtils.isTrue(value);
    }

    @Override
    public Date toDate() {
        if (value instanceof Date) return ((Date) value);
        return DateTimeUtils.formatUnknownString2Date(toString());
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
