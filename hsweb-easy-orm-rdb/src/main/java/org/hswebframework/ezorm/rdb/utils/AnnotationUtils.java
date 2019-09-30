package org.hswebframework.ezorm.rdb.utils;

import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Consumer;

public class AnnotationUtils {

    public static Set<Annotation> getAnnotations(Class entityClass, PropertyDescriptor descriptor) {
        Set<Annotation> annotations = new HashSet<>();
        Set<Class<? extends Annotation>> types = new HashSet<>();

        Consumer<Annotation[]> annoConsumer =(ann)->{
            for (Annotation annotation : ann) {
                if(!types.contains(annotation.getClass())){
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

        //获取属性
        while (true) {
            try {
                Field field = entityClass.getDeclaredField(descriptor.getName());
                annoConsumer.accept(field.getAnnotations());
                break;
            } catch (NoSuchFieldException e) {
                entityClass = entityClass.getSuperclass();
                if (entityClass == null || entityClass == Object.class) {
                    break;
                }
            }
        }


        return annotations;
    }

    public static <T extends Annotation> T getAnnotation(Class entityClass, PropertyDescriptor descriptor, Class<T> type) {
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
        while (ann == null) {
            try {
                Field field = entityClass.getDeclaredField(descriptor.getName());
                ann = field.getAnnotation(type);
                break;
            } catch (NoSuchFieldException e) {
                entityClass = entityClass.getSuperclass();
                if (entityClass == null || entityClass == Object.class) {
                    break;
                }
            }
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
