package org.hswebframework.ezorm.core;

import org.hswebframework.utils.StringUtils;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class MethodReferenceConverter {

    private static final Map<Class, String> cache = new ConcurrentHashMap<>();

    public static <T> String convertToColumn(MethodReferenceColumn<T> column) {
        return convertToColumn((Object) column);
    }

    public static <T> String convertToColumn(StaticMethodReferenceColumn<T> column) {
        return convertToColumn((Object) column);
    }

    public static String convertToColumn(Object column) {
        return cache.computeIfAbsent(column.getClass(), t -> {
            SerializedLambda lambda = SerializedLambda.of(column);

            String methodName = lambda.getMethodName();
            if (methodName.startsWith("get")) {
                return StringUtils.toLowerCaseFirstOne(methodName.substring(3));
            } else if (methodName.startsWith("is")) {
                return StringUtils.toLowerCaseFirstOne(methodName.substring(2));
            }
            return methodName;
        });
    }
}
