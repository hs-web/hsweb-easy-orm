package org.hswebframework.ezorm.rdb.config;

import org.hswebframework.ezorm.core.ObjectPropertyOperator;
import org.hswebframework.ezorm.rdb.utils.ApacheCommonPropertyOperator;

public class GlobalConfig {

    private static ObjectPropertyOperator propertyOperator = ApacheCommonPropertyOperator.INSANCE;

    public static ObjectPropertyOperator getPropertyOperator() {
        return propertyOperator;
    }
}
