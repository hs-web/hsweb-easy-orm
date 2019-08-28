package org.hswebframework.ezorm.rdb.meta;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.meta.ColumnMetaData;
import org.hswebframework.ezorm.core.meta.ObjectType;
import org.hswebframework.ezorm.core.meta.SchemaMetaData;

import java.util.*;
import java.util.stream.Collectors;

@Getter
@Setter
public abstract class AbstractDQLObjectMetaData implements DQLObjectMetaData {

    private String name;

    private String alias;

    private SchemaMetaData schema;

    @Override
    public abstract ObjectType getObjectType();

    private Map<String, ColumnMetaData> allColumns = new HashMap<>();
    private Map<String, ColumnMetaData> aliasColumns = new HashMap<>();

    private List<ForeignKeyMetadata> foreignKey = new ArrayList<>();

    @Override
    public SchemaMetaData getSchema() {
        return schema;
    }

    @Override
    public List<ColumnMetaData> getColumns() {
        return new ArrayList<>(allColumns.values());
    }

    @Override
    public List<ColumnMetaData> findColumns() {

        return allColumns
                .values()
                .stream()
                .flatMap(c -> getForeignKey()
                        .stream()
                        .map(ForeignKeyMetadata::getTarget)
                        .map(DQLObjectMetaData::getColumns)
                        .flatMap(Collection::stream))
                .collect(Collectors.toList());
    }

    @Override
    public Optional<ColumnMetaData> getColumn(String name) {
        Objects.requireNonNull(name, "name");
        return Optional.ofNullable(allColumns.getOrDefault(name, aliasColumns.get(name)));
    }

    @Override
    public Optional<ColumnMetaData> findColumn(String name) {

        return Optional.of(getColumn(name))
                .filter(Optional::isPresent)
                .orElseGet(() -> findNestColumn(name));
    }

    protected Optional<ColumnMetaData> findNestColumn(String name) {
        //table.name
        //schema.table.name
        if (name.contains(".")) {
            String[] arr = name.split("[,]");
            if (arr.length == 2) {
                return schema
                        .<DQLObjectMetaData>getObject(getObjectType(), arr[0])
                        .flatMap(meta -> meta.getColumn(arr[1]));
            } else if (arr.length == 3) {
                return schema.getDatabase()
                        .getSchema(arr[0])
                        .flatMap(another -> another
                                .<DQLObjectMetaData>getObject(getObjectType(), arr[0])
                                .flatMap(meta -> meta.getColumn(arr[1])));
            }
        }
        return Optional.empty();
    }

    @Override
    public List<ForeignKeyMetadata> getForeignKey() {
        return new ArrayList<>(foreignKey);
    }

    @Override
    public Optional<ForeignKeyMetadata> getForeignKey(String targetName) {
        return foreignKey
                .stream()
                .filter(key -> key.getTarget().equalsNameOrAlias(targetName))
                .findFirst();
    }


}
