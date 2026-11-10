package org.hswebframework.ezorm.rdb.mapping.annotation;

import java.lang.annotation.*;

/**
 * @see org.hswebframework.ezorm.rdb.codec.JsonValueCodec
 * @see Codec
 */
@Target({ElementType.FIELD, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Codec
public @interface JsonCodec {
    /** Preserve explicit nested null values for this column without changing other codecs. */
    boolean includeNulls() default false;
}
