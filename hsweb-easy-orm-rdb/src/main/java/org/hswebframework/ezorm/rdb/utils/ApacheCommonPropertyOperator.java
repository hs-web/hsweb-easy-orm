package org.hswebframework.ezorm.rdb.utils;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.beanutils.BeanUtilsBean;
import org.apache.commons.beanutils.PropertyUtilsBean;
import org.hswebframework.ezorm.core.ObjectPropertyOperator;

import java.util.Optional;

@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ApacheCommonPropertyOperator implements ObjectPropertyOperator {

    private static PropertyUtilsBean propertyUtils = BeanUtilsBean.getInstance().getPropertyUtils();

    public static final ObjectPropertyOperator INSANCE = new ApacheCommonPropertyOperator();

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
        try {
            propertyUtils.setProperty(object, name, value);
        } catch (NoSuchMethodException ignore) {

        }
    }
}
