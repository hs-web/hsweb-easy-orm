package org.hswebframework.ezorm.rdb.mapping.annotation;

import java.lang.annotation.*;

/**
 * @see org.hswebframework.ezorm.rdb.codec.EnumValueCodec
 */
@Target({ElementType.FIELD, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Codec
public @interface EnumCodec {

    String
        NAME = "name",
        ORDINAL = "ordinal";

    /**
     * @return 是否使用将枚举的序号进行位掩码以实现多选
     * @see java.sql.JDBCType#NUMERIC
     * @see Long
     */
    boolean toMask() default false;

    /**
     * 使用指定的枚举属性作为数据库的值,默认{@link Enum#name()}
     *
     * @return value
     * @see Enum#name()
     * @see Enum#ordinal()
     */
    String property() default NAME;

}
