package org.hswebframework.ezorm.rdb.supports.json;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class JsonValueCondition {

    private String path;

    private String termType = TermType.eq;

    private Object value;

    public static JsonValueCondition of(String path, Object value) {
        return of(path, TermType.eq, value);
    }

    public static JsonValueCondition of(String path, String termType, Object value) {
        return new JsonValueCondition(path, termType, value);
    }

    @SuppressWarnings("all")
    public static JsonValueCondition of(Term term) {
        Object value = term.getValue();
        if (value instanceof JsonValueCondition condition) {
            return condition;
        }
        String optionTermType = term.getOptions().isEmpty() ? null : term.getOptions().get(0);
        if (value instanceof Map) {
            Map<Object, Object> map = ((Map<Object, Object>) value);
            Object path = first(map, "path", "key", "name");
            Object termType = first(map, "termType", "term_type", "operator", "op");
            Object conditionValue = first(map, "value", "val");
            return new JsonValueCondition(
                path == null ? null : String.valueOf(path),
                termType == null ? (optionTermType == null ? TermType.eq : optionTermType) : String.valueOf(termType),
                conditionValue
            );
        }
        if (value instanceof Collection) {
            Iterator<?> iterator = ((Collection<?>) value).iterator();
            Object path = iterator.hasNext() ? iterator.next() : null;
            Object conditionValue = iterator.hasNext() ? iterator.next() : null;
            Object termType = iterator.hasNext() ? iterator.next() : null;
            return new JsonValueCondition(
                path == null ? null : String.valueOf(path),
                termType == null ? (optionTermType == null ? TermType.eq : optionTermType) : String.valueOf(termType),
                conditionValue
            );
        }
        if (value instanceof Object[]) {
            Object[] arr = ((Object[]) value);
            return new JsonValueCondition(
                arr.length > 0 && arr[0] != null ? String.valueOf(arr[0]) : null,
                arr.length > 2 && arr[2] != null ? String.valueOf(arr[2]) : (optionTermType == null ? TermType.eq : optionTermType),
                arr.length > 1 ? arr[1] : null
            );
        }
        return new JsonValueCondition(null, optionTermType == null ? TermType.eq : optionTermType, value);
    }

    private static Object first(Map<Object, Object> map, String... keys) {
        for (String key : keys) {
            if (map.containsKey(key)) {
                return map.get(key);
            }
        }
        return null;
    }
}
