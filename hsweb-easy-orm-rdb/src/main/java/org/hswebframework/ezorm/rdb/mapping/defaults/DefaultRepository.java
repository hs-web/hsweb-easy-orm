package org.hswebframework.ezorm.rdb.mapping.defaults;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.GlobalConfig;
import org.hswebframework.ezorm.core.ObjectPropertyOperator;
import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.LazyEntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.mapping.events.MappingEventTypes;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperator;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertResultOperator;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.SaveOrUpdateOperator;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.SaveResultOperator;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.UpsertOperator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;
import java.util.stream.Stream;

import static org.hswebframework.ezorm.rdb.events.ContextKeys.tableMetadata;
import static org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys.*;

public abstract class DefaultRepository<E> {

    protected DatabaseOperator operator;

    protected ResultWrapper<E, ?> wrapper;

    private volatile String idColumn;

    @Getter
    protected EntityColumnMapping mapping;

    @Setter
    protected volatile String[] properties;

    protected Supplier<RDBTableMetadata> tableSupplier;

    @Getter
    @Setter
    private ObjectPropertyOperator propertyOperator = GlobalConfig.getPropertyOperator();

    public DefaultRepository(DatabaseOperator operator, Supplier<RDBTableMetadata> supplier, ResultWrapper<E, ?> wrapper) {
        this.operator = operator;
        this.wrapper = wrapper;
        this.tableSupplier = supplier;
    }

    protected RDBTableMetadata getTable() {
        return tableSupplier.get();
    }

    public String[] getProperties() {
        if (properties == null) {
            properties = mapping.getColumnPropertyMapping()
                    .entrySet()
                    .stream()
                    .filter(kv -> getTable().getColumn(kv.getKey()).isPresent())
                    .map(Map.Entry::getValue)
                    .toArray(String[]::new);
        }
        return properties;
    }

    protected String getIdColumn() {
        if (idColumn == null) {
            this.idColumn = getTable().getColumns().stream()
                    .filter(RDBColumnMetadata::isPrimaryKey)
                    .findFirst()
                    .map(RDBColumnMetadata::getName)
                    .orElseThrow(() -> new UnsupportedOperationException("id column not exists"));
        }
        return idColumn;
    }

    protected void initMapping(Class<E> entityType) {

        this.mapping = LazyEntityColumnMapping.of(() -> getTable()
                .<EntityColumnMapping>findFeature(MappingFeatureType.columnPropertyMapping.createFeatureId(entityType))
                .orElseThrow(() -> new UnsupportedOperationException("unsupported columnPropertyMapping feature")));

    }

    protected SaveResultOperator doSave(Collection<E> data) {
        RDBTableMetadata table = getTable();
        UpsertOperator upsert = operator.dml().upsert(table.getFullName());

//        table.fireEvent(MappingEventTypes.insert_before,
//                ctx -> ctx.set(instance(batch), type("batch"), tableMetadata(table), insert(insert)));

        upsert.columns(getProperties());

        for (E e : data) {
            upsert.values(Stream.of(getProperties())
                    .map(property -> getInsertColumnValue(e,property))
                    .toArray());
        }
        return upsert.execute();
    }

    protected InsertResultOperator doInsert(E data) {
        RDBTableMetadata table = getTable();
        InsertOperator insert = operator.dml().insert(table.getFullName());

        table.fireEvent(MappingEventTypes.insert_before,
                ctx -> ctx.set(instance(data), type("single"), tableMetadata(table), insert(insert)));

        for (Map.Entry<String, String> entry : mapping.getColumnPropertyMapping().entrySet()) {
            String column = entry.getKey();
            String property = entry.getValue();

            insert.value(column, getInsertColumnValue(data,property));
        }

        return insert.execute();

    }

    private Object getInsertColumnValue(E data, String property) {
        Object value = propertyOperator.getProperty(data, property).orElse(null);
        if (value == null) {
            value = mapping.getColumnByProperty(property)
                    .map(RDBColumnMetadata::getDefaultValue)
                    .filter(RuntimeDefaultValue.class::isInstance)
                    .map(RuntimeDefaultValue.class::cast)
                    .map(RuntimeDefaultValue::get)
                    .orElse(null);
            if (value != null) {
                //回填
                propertyOperator.setProperty(data, property, value);
            }
        }
        return value;
    }

    protected InsertResultOperator doInsert(Collection<E> batch) {
        RDBTableMetadata table = getTable();
        InsertOperator insert = operator.dml().insert(table.getFullName());

        table.fireEvent(MappingEventTypes.insert_before,
                ctx -> ctx.set(instance(batch), type("batch"), tableMetadata(table), insert(insert)));

        insert.columns(getProperties());

        for (E e : batch) {
            insert.values(Stream.of(getProperties())
                    .map(property -> getInsertColumnValue(e,property))
                    .toArray());
        }
        return insert.execute();
    }

}
