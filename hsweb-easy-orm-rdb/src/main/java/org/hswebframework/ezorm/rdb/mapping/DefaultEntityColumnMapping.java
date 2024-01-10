package org.hswebframework.ezorm.rdb.mapping;

import lombok.Getter;
import lombok.Setter;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;

import java.lang.reflect.Constructor;
import java.util.*;

public class DefaultEntityColumnMapping implements EntityColumnMapping {

    private final Map<String, String> propertyColumnMapping = new LinkedHashMap<>();

    private final Map<String, String> columnPropertyMapping = new LinkedHashMap<>();

    @Getter
    private final String id;

    @Getter
    private final String name;

    @Getter
    private final TableOrViewMetadata table;

    @Getter
    private final Class<?> entityType;

    private final Constructor<?> constructor;

    public void addMapping(String column, String property) {
        columnPropertyMapping.put(column, property);
        propertyColumnMapping.put(property, column);
    }

    @SneakyThrows
    public DefaultEntityColumnMapping(TableOrViewMetadata table, Class<?> entityType) {
        this.id = getType().createFeatureId(entityType);
        this.name = getType().getName() + ":" + entityType.getSimpleName();
        this.table = table;
        this.entityType = entityType;
        constructor = this.entityType.getConstructor();
    }

    @Override
    public Optional<RDBColumnMetadata> getColumnByProperty(String property) {
        if (property.contains(".")) {
            String[] key = property.split("[.]");

            return table
                    .getForeignKey(key[0])
                    .flatMap(keyMetadata -> keyMetadata.getTarget().getColumn(key[1]));

        }
        return Optional
                .ofNullable(propertyColumnMapping.get(property))
                .flatMap(table::getColumn);
    }

    @Override
    public Optional<String> getPropertyByColumnName(String columnName) {
        return Optional
                .ofNullable(columnPropertyMapping.get(columnName));
    }

    @Override
    public Optional<RDBColumnMetadata> getColumnByName(String columnName) {
        if (columnName.contains(".")) {
            String[] key = columnName.split("[.]");

            return table.getForeignKey(key[0])
                        .flatMap(keyMetadata -> keyMetadata.getTarget().getColumn(key[1]));

        }
        return table.getColumn(columnName);
    }

    @Override
    public void reload() {
        for (RDBColumnMetadata column : table.getColumns()) {
            addMapping(column.getName(),column.getAlias());
        }
    }

    @Override
    public Map<String, String> getColumnPropertyMapping() {
        return Collections.unmodifiableMap(columnPropertyMapping);
    }

    @Override
    @SneakyThrows
    public Object newInstance() {
        return constructor.newInstance();
    }
}
