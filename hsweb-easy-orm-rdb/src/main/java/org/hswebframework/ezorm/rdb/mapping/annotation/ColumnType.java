package org.hswebframework.ezorm.rdb.mapping.annotation;

import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;

import java.lang.annotation.*;
import java.sql.JDBCType;

/**
 * @see DataType
 * @see RDBColumnMetadata
 */
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
     * @see DataType#getSqlType()
     */
    JDBCType jdbcType() default JDBCType.VARCHAR;

    /**
     * @return 自定义java类型
     * @see RDBColumnMetadata#getJavaType()
     */
    Class javaType() default Void.class;

    /**
     * @return DataType class
     * @see DataType
     * @see org.hswebframework.ezorm.rdb.metadata.JdbcDataType
     * @see RDBColumnMetadata#getJavaType()
     */
    Class<? extends DataType> type() default DataType.class;

}
