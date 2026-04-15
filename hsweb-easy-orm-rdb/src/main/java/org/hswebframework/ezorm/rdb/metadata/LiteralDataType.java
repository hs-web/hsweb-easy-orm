package org.hswebframework.ezorm.rdb.metadata;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.hswebframework.ezorm.rdb.metadata.dialect.DataTypeBuilder;

import java.sql.SQLType;

@Getter
@AllArgsConstructor(staticName = "of")
public class LiteralDataType implements DataType, DataTypeBuilder {

    private final String id;

    private final String name;

    private final SQLType sqlType;

    private final Class<?> javaType;

    private final boolean lengthSupport;

    private final boolean scaleSupport;

    @Override
    public String createColumnDataType(RDBColumnMetadata columnMetaData) {
        return name;
    }

    @Override
    public boolean isLengthSupport() {
        return lengthSupport;
    }

    @Override
    public boolean isScaleSupport() {
        return scaleSupport;
    }
}
