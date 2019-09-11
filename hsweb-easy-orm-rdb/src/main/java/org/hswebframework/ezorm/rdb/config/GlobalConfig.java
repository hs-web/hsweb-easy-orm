package org.hswebframework.ezorm.rdb.config;

import org.hswebframework.ezorm.core.ObjectConverter;
import org.hswebframework.ezorm.core.ObjectPropertyOperator;
import org.hswebframework.ezorm.rdb.utils.ApacheCommonPropertyOperator;

public class GlobalConfig {

    private static ObjectPropertyOperator propertyOperator;

    static {
        setPropertyOperator(ApacheCommonPropertyOperator.INSTANCE);
    }

    private static ObjectConverter objectConverter;

    public static ObjectConverter getObjectConverter() {
        return objectConverter;
    }

    public static ObjectPropertyOperator getPropertyOperator() {
        return propertyOperator;
    }

    public static void setPropertyOperator(ObjectPropertyOperator propertyOperator) {
        GlobalConfig.propertyOperator = propertyOperator;
    }

    public static void setObjectConverter(ObjectConverter objectConverter) {
        GlobalConfig.objectConverter = objectConverter;
    }
}
