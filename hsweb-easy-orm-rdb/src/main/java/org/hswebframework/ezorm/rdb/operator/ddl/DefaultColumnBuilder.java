package org.hswebframework.ezorm.rdb.operator.ddl;

import org.hswebframework.ezorm.core.DefaultValue;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.ValueCodecFactory;

import java.util.function.Consumer;

/**
 * @author zhouhao
 */
public class DefaultColumnBuilder implements ColumnBuilder {
    private final RDBColumnMetadata columnMetaData;
    private final TableBuilder tableBuilder;
    private final RDBTableMetadata tableMetaData;

    public DefaultColumnBuilder(RDBColumnMetadata columnMetaData, TableBuilder tableBuilder, RDBTableMetadata tableMetaData) {
        this.columnMetaData = columnMetaData;
        this.tableBuilder = tableBuilder;
        this.tableMetaData = tableMetaData;
    }

    @Override
    public ColumnBuilder name(String name) {
        columnMetaData.setName(name);
        return this;
    }

    @Override
    public ColumnBuilder custom(Consumer<RDBColumnMetadata> consumer) {
        consumer.accept(columnMetaData);
        return this;
    }

    @Override
    public ColumnBuilder alias(String name) {
        columnMetaData.setAlias(name);
        return this;
    }

    @Override
    public ColumnBuilder type(String typeId) {
        type(columnMetaData.getDialect().convertDataType(typeId));
        return this;
    }

    @Override
    public ColumnBuilder dataType(String dataType) {
        columnMetaData.setDataType(dataType);
        return this;
    }

    @Override
    public ColumnBuilder type(DataType type) {
        columnMetaData.setType(type);
        return this;
    }

    @Override
    public ColumnBuilder comment(String comment) {
        columnMetaData.setComment(comment);
        return this;
    }

    @Override
    public ColumnBuilder notNull() {
        columnMetaData.setNotNull(true);
        return this;
    }

    @Override
    public ColumnBuilder primaryKey() {
        columnMetaData.setPrimaryKey(true);
        return this;
    }

    @Override
    public ColumnBuilder columnDef(String def) {
        columnMetaData.setColumnDefinition(def);
        return this;
    }

    @Override
    public ColumnBuilder defaultValue(DefaultValue value) {
        columnMetaData.setDefaultValue(value);
        return this;
    }

    @Override
    public ColumnBuilder length(int len) {
        columnMetaData.setLength(len);
        return this;
    }

    @Override
    public ColumnBuilder length(int precision, int scale) {
        columnMetaData.setLength(precision);
        columnMetaData.setScale(scale);
        columnMetaData.setPrecision(precision);
        return this;
    }

    @Override
    public ColumnBuilder property(String propertyName, Object value) {
        columnMetaData.setProperty(propertyName, value);
        return this;
    }

    @Override
    public TableBuilder commit() {
        if(columnMetaData.getValueCodec()==null){
            tableMetaData.findFeature(ValueCodecFactory.ID)
                    .flatMap(factory -> factory.createValueCodec(columnMetaData))
                    .ifPresent(columnMetaData::setValueCodec);
        }

        tableMetaData.addColumn(columnMetaData);
        return tableBuilder;
    }
}
