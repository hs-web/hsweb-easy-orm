package org.hswebframework.ezorm.rdb.utils;

import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.*;
import java.util.function.Consumer;

public class AnnotationUtils {

    public static Optional<Field> getFiledByDescriptor(Class<?> type, PropertyDescriptor descriptor) {
        String name = descriptor.getName();
        //获取属性
        while (true) {
            try {
                try {
                    return Optional
                            .of(
                                    type.getDeclaredField(name)
                            );
                } catch (NoSuchFieldException e1) {
                    char[] arr = name.toCharArray();
                    if (Character.isUpperCase(arr[0])) {
                        arr[0] = Character.toLowerCase(arr[0]);
                        name = new String(arr);
                        continue;
                    }
                    throw e1;
                }
            } catch (NoSuchFieldException e) {
                type = type.getSuperclass();
                if (type == null || type == Object.class) {
                    break;
                }
            }
        }
        return Optional.empty();
    }

    public static Set<Annotation> getAnnotations(Class<?> entityClass, PropertyDescriptor descriptor) {
        Set<Annotation> annotations = new LinkedHashSet<>();
        Set<Class<? extends Annotation>> types = new LinkedHashSet<>();

        Consumer<Annotation[]> annoConsumer = (ann) -> {
            for (Annotation annotation : ann) {
                if (!types.contains(annotation.getClass())) {
                    annotations.add(annotation);
                }
                types.add(annotation.annotationType());
            }
        };

        //先获取方法
        Method read = descriptor.getReadMethod(),
                write = descriptor.getWriteMethod();
        if (read != null) {
            annoConsumer.accept(read.getAnnotations());
        }
        if (write != null) {
            annoConsumer.accept(write.getAnnotations());
        }

        getFiledByDescriptor(entityClass, descriptor)
                .map(Field::getAnnotations)
                .ifPresent(annoConsumer);

        return annotations;
    }

    public static <T extends Annotation> T getAnnotation(Class<?> entityClass, PropertyDescriptor descriptor, Class<T> type) {
        T ann = null;
        if (descriptor == null) {
            return null;
        }
        //先获取方法
        Method read = descriptor.getReadMethod(),
                write = descriptor.getWriteMethod();
        if (read != null) {
            ann = getAnnotation(read, type);
        }
        if (null == ann && write != null) {
            ann = getAnnotation(write, type);
        }
        //获取属性
        if (ann == null) {
            ann = getFiledByDescriptor(entityClass, descriptor)
                    .map(f -> f.getAnnotation(type))
                    .orElse(null);
        }

        return ann;
    }

    public static <T extends Annotation> T getAnnotation(Class<?> clazz, Class<T> annotation) {
        T ann = clazz.getAnnotation(annotation);

        while (ann == null) {
            clazz = clazz.getSuperclass();
            if (clazz == null || clazz == Object.class) {
                break;
            }
            ann = clazz.getAnnotation(annotation);
        }

        return ann;
    }


    public static <T extends Annotation> T getAnnotation(Method method, Class<T> annotation) {
        T ann = method.getAnnotation(annotation);

        Class<?> clazz = method.getDeclaringClass();

        while (ann == null) {
            clazz = clazz.getSuperclass();
            if (clazz == null || clazz == Object.class) {
                break;
            }
            try {
                //父类方法
                Method suMethod = clazz.getMethod(method.getName(), method.getParameterTypes());
                ann = suMethod.getAnnotation(annotation);
            } catch (NoSuchMethodException e) {
                return null;
            }
        }
        return ann;
    }
}
