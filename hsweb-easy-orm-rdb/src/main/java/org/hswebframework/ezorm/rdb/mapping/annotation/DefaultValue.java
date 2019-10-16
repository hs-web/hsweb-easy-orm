package org.hswebframework.ezorm.rdb.mapping.annotation;


import org.hswebframework.ezorm.core.DefaultValueGenerator;

import java.lang.annotation.*;

/**
 * @see org.hswebframework.ezorm.core.DefaultValueGenerator
 */
@Target({ElementType.FIELD, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
public @interface DefaultValue {

    /**
     * @return 生成器ID
     * @see DefaultValueGenerator#getId()
     */
    String generator();


}
