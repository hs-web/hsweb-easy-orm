package org.hswebframework.ezorm.rdb.metadata;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.metadata.dialect.DataTypeBuilder;

import java.sql.SQLType;
import java.util.function.Function;

@AllArgsConstructor(staticName = "of")
public class DataTypeBuilderSupport implements DataType, DataTypeBuilder {

    private DataType parent;

    private Function<RDBColumnMetadata, String> builder;

    @Override
    public String getId() {
        return parent.getId();
    }

    @Override
    public String getName() {
        return parent.getName();
    }

    @Override
    public SQLType getSqlType() {
        return parent.getSqlType();
    }

    @Override
    public Class getJavaType() {
        return parent.getJavaType();
    }

    @Override
    public String createColumnDataType(RDBColumnMetadata columnMetaData) {
        return builder.apply(columnMetaData);
    }
}
