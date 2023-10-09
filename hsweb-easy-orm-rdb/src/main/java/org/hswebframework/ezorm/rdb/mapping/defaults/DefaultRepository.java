package org.hswebframework.ezorm.rdb.mapping.defaults;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.GlobalConfig;
import org.hswebframework.ezorm.core.ObjectPropertyOperator;
import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.rdb.events.ContextKey;
import org.hswebframework.ezorm.rdb.events.ContextKeyValue;
import org.hswebframework.ezorm.rdb.events.ContextKeys;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.LazyEntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.mapping.events.EventResultOperator;
import org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys;
import org.hswebframework.ezorm.rdb.mapping.events.MappingEventTypes;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperator;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertResultOperator;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.SaveOrUpdateOperator;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.SaveResultOperator;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.UpsertOperator;
import reactor.core.publisher.Mono;

import java.lang.reflect.Array;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
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

    protected final List<ContextKeyValue<?>> defaultContextKeyValue = new ArrayList<>();

    public DefaultRepository(DatabaseOperator operator, Supplier<RDBTableMetadata> supplier, ResultWrapper<E, ?> wrapper) {
        this.operator = operator;
        this.wrapper = wrapper;
        this.tableSupplier = supplier;
        defaultContextKeyValue.add(repository.value(this));
        defaultContextKeyValue.add(ContextKeys.database.value(operator));

    }

    protected RDBTableMetadata getTable() {
        return tableSupplier.get();
    }

    protected ContextKeyValue<?>[] getDefaultContextKeyValue(ContextKeyValue<?>... kv) {
        if (kv.length == 0) {
            return defaultContextKeyValue.toArray(new ContextKeyValue[0]);
        }
        List<ContextKeyValue<?>> keyValues = new ArrayList<>(defaultContextKeyValue);
        keyValues.addAll(Arrays.asList(kv));
        return keyValues.toArray(new ContextKeyValue[0]);
    }

    public String[] getProperties() {
        if (properties == null) {
            properties = mapping
                    .getColumnPropertyMapping()
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
            this.idColumn = getTable()
                    .getColumns()
                    .stream()
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
        defaultContextKeyValue.add(MappingContextKeys.columnMapping(mapping));
    }

    protected SaveResultOperator doSave(Collection<E> data) {
        RDBTableMetadata table = getTable();
        UpsertOperator upsert = operator.dml().upsert(table.getFullName());
        int finalPrimaryIndex = getPrimaryKeyIndex(table.getColumns());
        return EventResultOperator.create(
                () -> {
                    upsert.columns(getProperties());
                    List<String> ignore = new ArrayList<>();
                    Set<Object> duplicatePrimary = new HashSet<>(32);
                    List<E> list = new ArrayList<>(data);
                    ListIterator<E> iterator = list.listIterator(list.size());
                    while (iterator.hasPrevious()) {
                        E e = iterator.previous();
                        Object[] array = Stream.of(getProperties())
                                               .map(property -> getInsertColumnValue(e, property, (prop, val) -> ignore.add(prop)))
                                               .toArray();
                        if (array.length > finalPrimaryIndex && !duplicatePrimary.add(array[finalPrimaryIndex])){
                            continue;
                        }
                        upsert.values(array);
                    }
                    upsert.ignoreUpdate(ignore.toArray(new String[0]));
                    return upsert.execute();
                },
                SaveResultOperator.class,
                table,
                MappingEventTypes.save_before,
                MappingEventTypes.save_after,
                getDefaultContextKeyValue(instance(data),
                                          type("batch"),
                                          tableMetadata(table),
                                          upsert(upsert))
        );
    }

    protected InsertResultOperator doInsert(E data) {
        RDBTableMetadata table = getTable();
        InsertOperator insert = operator.dml().insert(table.getFullName());

        return EventResultOperator.create(
                () -> {
                    for (Map.Entry<String, String> entry : mapping.getColumnPropertyMapping().entrySet()) {
                        String column = entry.getKey();
                        String property = entry.getValue();
                        insert.value(column, getInsertColumnValue(data, property));
                    }
                    return insert.execute();
                },
                InsertResultOperator.class,
                table,
                MappingEventTypes.insert_before,
                MappingEventTypes.insert_after,
                getDefaultContextKeyValue(
                        instance(data),
                        type("single"),
                        tableMetadata(table),
                        insert(insert))
        );

    }

    private Object getInsertColumnValue(E data, String property, BiConsumer<String, Object> whenDefaultValue) {
        Object value = GlobalConfig.getPropertyOperator().getProperty(data, property).orElse(null);
        if (value == null) {
            value = mapping.getColumnByProperty(property)
                           .flatMap(RDBColumnMetadata::generateDefaultValue)
                           .orElse(null);
            if (value != null) {
                whenDefaultValue.accept(property, value);
                //回填
                if(!(value instanceof NativeSql)){
                    GlobalConfig.getPropertyOperator().setProperty(data, property, value);
                }
            }
        }
        return value;
    }

    private Object getInsertColumnValue(E data, String property) {

        return getInsertColumnValue(data, property, (prop, val) -> {
        });
    }

    protected InsertResultOperator doInsert(Collection<E> batch) {
        RDBTableMetadata table = getTable();
        InsertOperator insert = operator.dml().insert(table.getFullName());
        int finalPrimaryIndex = getPrimaryKeyIndex(table.getColumns());

        return EventResultOperator.create(
                () -> {
                    insert.columns(getProperties());
                    Set<Object> duplicatePrimary = new HashSet<>(32);
                    List<E> list = new ArrayList<>(batch);
                    ListIterator<E> iterator = list.listIterator(list.size());
                    while (iterator.hasPrevious()) {
                        E e = iterator.previous();
                        Object[] array = Stream.of(getProperties())
                                               .map(property -> getInsertColumnValue(e, property))
                                               .toArray();
                        if (array.length > finalPrimaryIndex && !duplicatePrimary.add(array[finalPrimaryIndex])){
                            continue;
                        }
                        insert.values(array);
                    }
                    return insert.execute();
                },
                InsertResultOperator.class,
                table,
                MappingEventTypes.insert_before,
                MappingEventTypes.insert_after,
                getDefaultContextKeyValue(
                        instance(batch),
                        type("batch"),
                        tableMetadata(table),
                        insert(insert))
        );
    }

    private int getPrimaryKeyIndex(List<RDBColumnMetadata> columns) {
        int index = 0;
        int primaryIndex = -1;
        for (RDBColumnMetadata column : columns) {
            if (column.isInsertable()) {
                if (column.isPrimaryKey()) {
                    primaryIndex = index;
                    break;
                }
            }
            index++;
        }
        return primaryIndex;
    }


}
