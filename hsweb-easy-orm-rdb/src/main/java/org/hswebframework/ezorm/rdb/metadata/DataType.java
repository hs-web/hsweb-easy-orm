package org.hswebframework.ezorm.rdb.metadata;

import java.sql.JDBCType;
import java.sql.SQLType;
import java.util.function.Function;

/**
 * @see JdbcDataType
 * @see CustomDataType
 */
public interface DataType {

    String getId();

    String getName();

    SQLType getSqlType();

    Class<?> getJavaType();

    default boolean isScaleSupport() {
        return getSqlType() == JDBCType.DECIMAL ||
                getSqlType() == JDBCType.DOUBLE ||
                getSqlType() == JDBCType.NUMERIC ||
                getSqlType() == JDBCType.FLOAT;
    }

    default boolean isLengthSupport() {
        return isScaleSupport() ||
                getSqlType() == JDBCType.VARCHAR ||
                getSqlType() == JDBCType.CHAR||
                getSqlType() == JDBCType.NVARCHAR
                ;
    }

    static DataType custom(String id, String name, SQLType sqlType, Class<?> javaType) {
        return CustomDataType.of(id, name, sqlType, javaType);
    }

    static DataType jdbc(JDBCType jdbcType, Class<?> javaType) {
        return JdbcDataType.of(jdbcType, javaType);
    }

    static DataType builder(DataType type, Function<RDBColumnMetadata, String> builder) {
        return DataTypeBuilderSupport.of(type, builder);
    }
}
