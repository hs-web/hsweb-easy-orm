package org.hswebframework.ezorm.core;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.beanutils.BeanUtilsBean;
import org.apache.commons.beanutils.PropertyUtilsBean;

import java.beans.PropertyDescriptor;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;

@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ApacheCommonPropertyOperator implements ObjectPropertyOperator, ObjectConverter {

    private static final PropertyUtilsBean propertyUtils = BeanUtilsBean.getInstance().getPropertyUtils();

    public static final ApacheCommonPropertyOperator INSTANCE = new ApacheCommonPropertyOperator();

    @Override
    public Optional<Object> getProperty(Object object, String name) {
        try {
            return Optional.ofNullable(propertyUtils.getProperty(object, name));
        } catch (NoSuchMethodException ignore) {

        } catch (Exception e) {
            ApacheCommonPropertyOperator.log.info("无法获取属性:{},对象:{}", name, object, e);
        }
        return Optional.empty();
    }

    @Override
    @SneakyThrows
    public void setProperty(Object object, String name, Object value) {
        try {
            BeanUtils.setProperty(object, name, value);
        } catch (Exception err) {
            log.warn(err.getMessage(), err);
        }
    }

    @Override
    @SneakyThrows
    public <T> T convert(Object from, Class<T> to) {
        T newInstance = to.getConstructor().newInstance();
        try {
            BeanUtils.copyProperties(newInstance, from);
        } catch (Exception err) {
            log.warn(err.getMessage(), err);
        }
        return newInstance;
    }

    @Override
    @SneakyThrows
    public <T> T convert(Object from, Supplier<T> to) {
        T instance = to.get();
        try {
            if (instance instanceof Map) {
                @SuppressWarnings("all")
                Map<Object, Object> mapValue = ((Map<Object, Object>) instance);
                for (PropertyDescriptor propertyDescriptor : BeanUtilsBean.getInstance().getPropertyUtils().getPropertyDescriptors(from)) {
                    mapValue.put(propertyDescriptor.getName(), BeanUtilsBean.getInstance().getPropertyUtils().getProperty(from, propertyDescriptor.getName()));
                }
                return instance;
            }
            BeanUtils.copyProperties(instance, from);
        } catch (Exception err) {
            log.warn(err.getMessage(), err);
        }
        return instance;
    }
}
