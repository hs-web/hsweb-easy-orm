package org.hswebframework.ezorm.rdb.supports.postgres;

import lombok.Getter;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.DataTypeBuilder;

import java.sql.JDBCType;
import java.sql.SQLType;


@Getter
public class JsonType implements DataType, DataTypeBuilder {

    public static JsonType INSTANCE = new JsonType();

    @Override
    public Class<?> getJavaType() {
        return String.class;
    }

    @Override
    public String getId() {
        return "json";
    }

    @Override
    public String getName() {
        return "json";
    }

    @Override
    public SQLType getSqlType() {
        return JDBCType.OTHER;
    }

    @Override
    public String createColumnDataType(RDBColumnMetadata columnMetaData) {
        return "json";
    }

}
