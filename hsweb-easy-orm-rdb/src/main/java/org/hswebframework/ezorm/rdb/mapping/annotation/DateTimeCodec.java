package org.hswebframework.ezorm.rdb.mapping.annotation;

import java.lang.annotation.*;

@Target({ElementType.METHOD, ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Codec
public @interface DateTimeCodec {
    String format() default "yyyy-MM-dd HH:mm:ss";
}
