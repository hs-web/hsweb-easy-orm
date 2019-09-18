package org.hswebframework.ezorm.rdb.utils;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.beanutils.BeanUtilsBean;
import org.apache.commons.beanutils.PropertyUtilsBean;
import org.hswebframework.ezorm.core.ObjectConverter;
import org.hswebframework.ezorm.core.ObjectPropertyOperator;

import java.util.Optional;
import java.util.function.Supplier;

@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ApacheCommonPropertyOperator implements ObjectPropertyOperator, ObjectConverter {

    private static PropertyUtilsBean propertyUtils = BeanUtilsBean.getInstance().getPropertyUtils();

    public static final ApacheCommonPropertyOperator INSTANCE = new ApacheCommonPropertyOperator();

    @Override
    public Optional<Object> getProperty(Object object, String name) {
        try {
            return Optional.ofNullable(propertyUtils.getProperty(object, name));
        } catch (NoSuchMethodException ignore) {

        } catch (Exception e) {
            log.info("无法获取属性:{},对象:{}", name, object, e);
        }
        return Optional.empty();
    }

    @Override
    @SneakyThrows
    public void setProperty(Object object, String name, Object value) {
        BeanUtilsBean.getInstance().setProperty(object, name, value);

    }

    @Override
    @SneakyThrows
    public <T> T convert(Object from, Class<T> to) {
        T newInstance = to.newInstance();
        BeanUtilsBean.getInstance().copyProperties(newInstance, from);
        return newInstance;
    }

    @Override
    @SneakyThrows
    public <T> T convert(Object from, Supplier<T> to) {
        T instance = to.get();
        BeanUtilsBean.getInstance().copyProperties(instance, from);
        return instance;
    }
}
