package org.hswebframework.ezorm.rdb.utils;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.beanutils.BeanUtilsBean;
import org.apache.commons.beanutils.PropertyUtilsBean;

import java.util.Optional;

@Slf4j
public class PropertiesUtils {
    private static PropertyUtilsBean propertyUtils = BeanUtilsBean.getInstance().getPropertyUtils();

    public static Optional<Object> getProperty(Object object, String propertyName) {

        try {
            return Optional.ofNullable(propertyUtils.getProperty(object, propertyName));
        } catch (NoSuchMethodException ignore) {

        } catch (Exception e) {
            log.debug("无法获取属性:{},对象:{}", propertyName, object, e);
        }

        return Optional.empty();
    }
}
