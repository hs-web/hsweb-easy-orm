package org.hswebframework.ezorm.core;

import org.hswebframework.utils.StringUtils;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class LambdaColumnConvert {

    private static final Map<Class, String> cache = new ConcurrentHashMap<>();

    public static <T> String convertToColumn(LambdaColumn<T> column) {
        return cache.computeIfAbsent(column.getClass(), t -> {
            SerializedLambda lambda = SerializedLambda.of(column);
            String methodName = lambda.getMethodName();
            if (methodName.startsWith("get")) {
                return StringUtils.toLowerCaseFirstOne(methodName.substring(3, methodName.length()));
            } else if (methodName.startsWith("is")) {
                return StringUtils.toLowerCaseFirstOne(methodName.substring(2, methodName.length()));
            }
            return methodName;
        });
    }
}
