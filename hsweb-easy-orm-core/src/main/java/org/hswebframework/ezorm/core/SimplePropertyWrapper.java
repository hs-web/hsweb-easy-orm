package org.hswebframework.ezorm.core;

import org.jetlinks.json.Json;
import org.jetlinks.json.JsonMapper;
import org.jetlinks.json.JsonType;
import org.hswebframework.ezorm.core.utils.StringUtils;
import org.hswebframework.utils.ClassUtils;
import org.hswebframework.utils.DateTimeUtils;
import org.hswebframework.utils.time.DateFormatter;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * 元数据属性的标量和 JSON 转换。已有目标类型直接返回；文本按 JSON 读取，其他对象交给 Provider 转换。
 * 默认 JSON 配置独立于 Spring 和 HTTP；需要业务配置时传入可共享的 JsonMapper。
 * @since 1.0
 */
public class SimplePropertyWrapper implements PropertyWrapper {

    private final Object value;

    private final JsonMapper mapper;

    public SimplePropertyWrapper(Object value) {
        this.value = value;
        this.mapper = null;
    }

    /** 使用指定配置转换属性，不复制已有的目标类型值。 */
    public SimplePropertyWrapper(Object value, JsonMapper mapper) {
        this.value = value;
        this.mapper = Objects.requireNonNull(mapper, "mapper");
    }

    private JsonMapper jsonMapper() {
        return mapper == null ? DefaultJson.MAPPER : mapper;
    }

    private static class DefaultJson {
        // 仅实际读取 JSON 时初始化，普通数字、日期属性不会构造映射器或占用应用全局位置。
        private static final JsonMapper MAPPER = Json.builder().provider("jackson2").build();
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
        if (value instanceof CharSequence) {
            String text = value.toString();
            return text.isBlank() ? null : jsonMapper().read(text, type);
        }
        return jsonMapper().convert(value, type);
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
        JsonType<List<T>> target = JsonType.listOf(JsonType.of(type));
        if (value instanceof CharSequence) {
            String text = value.toString();
            return text.isBlank() ? null : jsonMapper().read(text, target);
        }
        return jsonMapper().convert(value, target);
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
