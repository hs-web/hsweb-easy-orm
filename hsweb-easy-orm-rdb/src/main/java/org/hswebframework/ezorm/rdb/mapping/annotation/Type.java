package org.hswebframework.ezorm.rdb.mapping.annotation;


import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.rdb.types.DataType;

import java.lang.annotation.*;

@Target({ElementType.FIELD, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
public @interface Type {

    String typeName() default "";

    Class<? extends DataType> type() default DataType.class;

    Class<?> elementType() default Void.class;

    Class<ValueCodec> codec() default ValueCodec.class;
}
