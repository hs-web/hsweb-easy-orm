package org.hswebframework.ezorm.core;

import org.hswebframework.ezorm.core.utils.StringUtils;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class MethodReferenceConverter {

    private static final Map<Class<?>, MethodReferenceInfo> cache = new ConcurrentHashMap<>();

    public static <T> String convertToColumn(MethodReferenceColumn<T> column) {
        return convertToColumn((Object) column);
    }

    public static <T> String convertToColumn(StaticMethodReferenceColumn<T> column) {
        return convertToColumn((Object) column);
    }

    public static <T, O> String convertToColumn(SetterMethodReferenceColumn<T, O> column) {
        return convertToColumn((Object) column);
    }

    public static MethodReferenceInfo parse(Object column) {
        Class<?> clazz = column.getClass();
        MethodReferenceInfo ref = cache.get(clazz);
        if (ref != null) {
            return ref;
        }
        return cache.computeIfAbsent(clazz, t -> {
            SerializedLambda lambda = SerializedLambda.of(column);
            String methodName = lambda.getMethodName();
            String columnName = methodName;
            if (methodName.startsWith("get") || methodName.startsWith("set")) {
                columnName = StringUtils.toLowerCaseFirstOne(methodName.substring(3));
            } else if (methodName.startsWith("is")) {
                columnName = StringUtils.toLowerCaseFirstOne(methodName.substring(2));
            }
            return new MethodReferenceInfo(columnName, methodName, lambda.getImplClass());
        });
    }

    public static String convertToColumn(Object column) {
        return parse(column).getColumn();
    }
}
