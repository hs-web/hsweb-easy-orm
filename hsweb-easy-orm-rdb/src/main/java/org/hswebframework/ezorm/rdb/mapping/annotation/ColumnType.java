package org.hswebframework.ezorm.rdb.mapping.annotation;

import org.hswebframework.ezorm.rdb.metadata.DataType;

import java.lang.annotation.*;
import java.sql.JDBCType;

@Target({ElementType.FIELD, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
public @interface ColumnType {

    /**
     * @return 类型标识
     * @see DataType#getId()
     */
    String typeId() default "";

    /**
     * @return JDBCType
     * @see DataType#getJdbcType()
     */
    JDBCType jdbcType() default JDBCType.OTHER;

    /**
     * @return DataType class
     * @see DataType
     * @see org.hswebframework.ezorm.rdb.metadata.JdbcDataType
     */
    Class<? extends DataType> type() default DataType.class;

}
