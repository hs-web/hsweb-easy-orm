package org.hswebframework.ezorm.rdb.supports.postgres;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.DataTypeBuilder;

import java.sql.JDBCType;
import java.sql.SQLType;

@Getter
@AllArgsConstructor
public class JsonbType implements DataType , DataTypeBuilder {
    public static JsonbType INSTANCE = new JsonbType();

    @Override
    public Class<?> getJavaType() {
        return String.class;
    }

    @Override
    public String getId() {
        return "jsonb";
    }

    @Override
    public String getName() {
        return "jsonb";
    }

    @Override
    public SQLType getSqlType() {
        return JDBCType.OTHER;
    }

    @Override
    public String createColumnDataType(RDBColumnMetadata columnMetaData) {
        return "jsonb";
    }
}
