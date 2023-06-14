package org.hswebframework.ezorm.rdb.utils;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.beanutils.BeanUtilsBean;
import org.apache.commons.beanutils.PropertyUtilsBean;

import java.beans.IndexedPropertyDescriptor;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Field;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Slf4j
public class PropertiesUtils {

    public static PropertyDescriptor[] getDescriptors(Class<?> clazz) {
        Map<String, Integer> nameShorts = new HashMap<>();
        int i = 0;
        Class<?> temp = clazz;
        int obj = 1;
        while (temp != Object.class) {
            for (Field declaredField : temp.getDeclaredFields()) {
                nameShorts.put(declaredField.getName(), i++);
            }
            temp = temp.getSuperclass();
            i = -(obj++ * 1000);
        }

        return Arrays
                .stream(BeanUtilsBean
                                .getInstance()
                                .getPropertyUtils()
                                .getPropertyDescriptors(clazz))
                .sorted(Comparator.comparingInt(p -> nameShorts.getOrDefault(p.getName(), Integer.MAX_VALUE)))
                .toArray(PropertyDescriptor[]::new);
    }


    @SuppressWarnings("all")
    public static List<Object> convertList(Object object) {
        if (object == null) {
            return Collections.emptyList();
        }
        if (object instanceof Object[]) {
            return Arrays.asList(((Object[]) object));
        }
        if (object instanceof Collection) {
            return new ArrayList<>(((Collection) object));
        }

        return Arrays.asList(object);
    }

    public static Optional<Field> getPropertyField(Class<?> clazz, String fieldName) {
        Field field = null;
        Class<?> tmp = clazz;
        do {
            try {
                field = tmp.getDeclaredField(fieldName);
            } catch (NoSuchFieldException e) {
                tmp = tmp.getSuperclass();
                if (tmp == null || tmp == Object.class) {
                    break;
                }
            }
        } while (field == null);

        return Optional.ofNullable(field);
    }
}
