package org.hswebframework.ezorm.rdb.mapping.annotation;

import java.lang.annotation.*;

@Target({ElementType.FIELD, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
public @interface Upsert {

    /**
     * 执行upsert操作时，是否只用于新增
     *
     * @return 是否可以更新
     */
    boolean insertOnly() default false;

}
