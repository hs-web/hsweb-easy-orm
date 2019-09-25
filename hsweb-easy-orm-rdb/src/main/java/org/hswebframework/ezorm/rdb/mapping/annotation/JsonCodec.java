package org.hswebframework.ezorm.rdb.mapping.annotation;

import java.lang.annotation.*;

/**
 * @see org.hswebframework.ezorm.rdb.codec.JsonValueCodec
 */
@Target({ElementType.FIELD, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Codec
public @interface JsonCodec {

}
