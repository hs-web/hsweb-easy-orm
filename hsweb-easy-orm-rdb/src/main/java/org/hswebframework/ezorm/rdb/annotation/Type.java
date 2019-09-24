package org.hswebframework.ezorm.rdb.annotation;


import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.rdb.types.DataType;
import org.hswebframework.ezorm.rdb.types.DataTypes;

import java.lang.annotation.*;

@Target({ElementType.FIELD, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
public @interface Type {
    DataTypes value();

    Class<?> elementType() default Void.class;

    Class<ValueCodec> codec() default ValueCodec.class;
}
