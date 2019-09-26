package org.hswebframework.ezorm.rdb.metadata;


import java.sql.JDBCType;
import java.util.function.Function;

/**
 * @see JdbcDataType
 * @see CustomDataType
 */
public interface DataType {

    String getId();

    String getName();

    JDBCType getJdbcType();

    Class getJavaType();

    static DataType custom(String id,String name,JDBCType jdbcType,Class javaType){
     return CustomDataType.of(id, name, jdbcType, javaType) ;
    }

    static DataType jdbc(JDBCType jdbcType, Class javaType) {
        return JdbcDataType.of(jdbcType, javaType);
    }

    static DataType builder(DataType type, Function<RDBColumnMetadata, String> builder) {
        return DataTypeBuilderSupport.of(type, builder);
    }
}
