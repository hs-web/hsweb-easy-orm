package org.hswebframework.ezorm.rdb.metadata;

import lombok.AllArgsConstructor;

import java.sql.SQLType;

@AllArgsConstructor
public class LengthSupportDataType implements LengthSupport, DataType {
    private final DataType dataType;
    private final int length, precision, scale;

    @Override
    public String getId() {
        return dataType.getId();
    }

    @Override
    public String getName() {
        return dataType.getName();
    }

    @Override
    public SQLType getSqlType() {
        return dataType.getSqlType();
    }

    @Override
    public Class<?> getJavaType() {
        return dataType.getJavaType();
    }

    @Override
    public int getLength() {
        return length;
    }

    @Override
    public int getScale() {
        return scale;
    }

    @Override
    public int getPrecision() {
        return precision;
    }

}
