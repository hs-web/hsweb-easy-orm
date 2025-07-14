package org.hswebframework.ezorm.rdb.mapping.defaults;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.rdb.events.ContextKeyValue;
import org.hswebframework.ezorm.rdb.events.ContextKeys;
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
import org.hswebframework.ezorm.rdb.operator.dml.upsert.SaveResultOperator;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.UpsertOperator;
import org.hswebframework.ezorm.rdb.utils.PropertyUtils;

import java.util.*;
import java.util.function.BiConsumer;
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

        this.mapping = LazyEntityColumnMapping
            .of(() -> getTable().findFeatureNow(MappingFeatureType.columnPropertyMapping.createFeatureId(entityType)));

        defaultContextKeyValue.add(MappingContextKeys.columnMapping(mapping));
    }

    protected Collection<E> tryMergeDuplicate(Collection<E> data) {
        if (data.isEmpty()) {
            return data;
        }
        Map<Object, E> merging = new HashMap<>(data.size());
        List<E> merged = new ArrayList<>(data.size());
        for (E datum : data) {
            Object id = getProperty(datum, getIdColumn());
            if (id == null) {
                merged.add(datum);
            } else {
                merging.compute(id, (_id, old) -> {
                    if (old != null) {
                        return merge(old, datum);
                    }
                    return datum;
                });
            }
        }
        merged.addAll(merging.values());
        return merged;
    }

    protected E merge(E older, E newer) {
        for (String property : getProperties()) {

            Object newerVal = getProperty(newer, property);
            if (newerVal != null) {
                continue;
            }

            newerVal = getProperty(older, property);
            if (newerVal != null) {
                setProperty(newer, property, newerVal);
            }
        }
        return newer;
    }

    void setProperty(E entity, String property, Object value) {
        PropertyUtils.setProperty(entity, property, value, mapping);
    }

    Object getProperty(E entity, String property) {
        return PropertyUtils.getProperty(entity, property, mapping).orElse(null);
    }

    protected SaveResultOperator doSave(Collection<E> data) {
        Collection<E> _data = tryMergeDuplicate(data);
        RDBTableMetadata table = getTable();
        UpsertOperator upsert = operator.dml().upsert(table);

        return EventResultOperator.create(
            () -> {
                upsert.columns(getProperties());
                List<String> ignore = new ArrayList<>();
                for (E e : _data) {
                    upsert.values(Stream.of(getProperties())
                                        .map(property -> getInsertColumnValue(e, property, (prop, val) -> ignore.add(prop)))
                                        .toArray());
                }
                upsert.ignoreUpdate(ignore.toArray(new String[0]));
                return upsert.execute();
            },
            SaveResultOperator.class,
            table,
            MappingEventTypes.save_before,
            MappingEventTypes.save_after,
            getDefaultContextKeyValue(instance(_data),
                                      type("batch"),
                                      tableMetadata(table),
                                      upsert(upsert))
        );
    }

    protected InsertResultOperator doInsert(E data) {
        RDBTableMetadata table = getTable();
        InsertOperator insert = operator.dml().insert(table);

        return EventResultOperator.create(
            () -> {
                insert.columns(getProperties());
                insert.values(Stream
                                  .of(getProperties())
                                  .map(property -> getInsertColumnValue(data, property))
                                  .toArray());
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
        Object value = getProperty(data, property);
        if (value == null) {
            value = mapping.getColumnByProperty(property)
                           .flatMap(RDBColumnMetadata::generateDefaultValue)
                           .orElse(null);
            if (value != null) {
                whenDefaultValue.accept(property, value);
                //回填
                if (!(value instanceof NativeSql)) {
                    setProperty(data, property, value);
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
        Collection<E> _data = tryMergeDuplicate(batch);
        RDBTableMetadata table = getTable();
        InsertOperator insert = operator.dml().insert(table);

        return EventResultOperator.create(
            () -> {
                insert.columns(getProperties());

                for (E e : _data) {
                    insert.values(Stream.of(getProperties())
                                        .map(property -> getInsertColumnValue(e, property))
                                        .toArray());
                }
                return insert.execute();
            },
            InsertResultOperator.class,
            table,
            MappingEventTypes.insert_before,
            MappingEventTypes.insert_after,
            getDefaultContextKeyValue(
                instance(_data),
                type("batch"),
                tableMetadata(table),
                insert(insert))
        );
    }

}
