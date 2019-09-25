package org.hswebframework.ezorm.rdb.mapping.annotation;

import java.lang.annotation.*;

@Target({ ElementType.ANNOTATION_TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
public @interface Codec {
}
