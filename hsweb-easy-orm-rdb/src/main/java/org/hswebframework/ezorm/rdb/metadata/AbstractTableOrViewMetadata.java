package org.hswebframework.ezorm.rdb.metadata;

import lombok.Getter;
import lombok.Setter;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.core.meta.ObjectMetadata;
import org.hswebframework.ezorm.rdb.codec.EnumValueCodec;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.metadata.key.ForeignKeyBuilder;
import org.hswebframework.ezorm.rdb.metadata.key.ForeignKeyMetadata;
import org.hswebframework.ezorm.rdb.metadata.key.LazyForeignKeyMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.query.JoinFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.query.QueryTermsFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.query.SelectColumnFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.query.SortOrderFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.EnumFragmentBuilder;
import org.hswebframework.ezorm.rdb.utils.FeatureUtils;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.Optional.*;

@Getter
@Setter
public abstract class AbstractTableOrViewMetadata implements TableOrViewMetadata {

    private String name;

    private String alias;

    private RDBSchemaMetadata schema;

    @Setter
    private Consumer<RDBColumnMetadata> onColumnAdded;

    protected Map<String, RDBColumnMetadata> allColumns = new ConcurrentHashMap<String, RDBColumnMetadata>() {
        @Override
        public RDBColumnMetadata get(Object key) {
            String k = String.valueOf(key);
            RDBColumnMetadata metadata = super.get(k);
            if (metadata == null) {
                metadata = super.get(k.toUpperCase());
            }
            if (metadata == null) {
                metadata = super.get(k.toLowerCase());
            }
            return metadata;
        }
    };

    protected List<ForeignKeyMetadata> foreignKey = new CopyOnWriteArrayList<>();

    protected Map<String, Feature> features = new ConcurrentHashMap<>();

    public AbstractTableOrViewMetadata() {
        //注册默认的where条件构造器
        addFeature(QueryTermsFragmentBuilder.of(this));
        //注册默认的查询列构造器
        addFeature(SelectColumnFragmentBuilder.of(this));
        //JOIN
        addFeature(JoinFragmentBuilder.of(this));
        //order
        addFeature(SortOrderFragmentBuilder.of(this));

    }

    public boolean isTable() {
        return this instanceof RDBTableMetadata;
    }

    public boolean isView() {
        return this instanceof RDBViewMetadata;
    }

    public void removeColumn(String name) {
        RDBColumnMetadata metadata = allColumns.remove(name);
        if (metadata != null) {
            allColumns.remove(metadata.getAlias());
        }
    }

    @Override
    public RDBSchemaMetadata getSchema() {
        return schema;
    }

    public void addColumn(RDBColumnMetadata column) {

        column.setOwner(this);
        allColumns.put(column.getName(), column);
        allColumns.put(column.getAlias(), column);
        if (onColumnAdded != null) {
            onColumnAdded.accept(column);
        }
    }


    @Override
    public List<RDBColumnMetadata> getColumns() {
        return new ArrayList<>(allColumns
                                       .values()
                                       .stream()
                                       .sorted()
                                       .collect(Collectors.toMap(RDBColumnMetadata::getName, Function.identity(), (_1, _2) -> _1))
                                       .values());
    }

    @Override
    public List<RDBColumnMetadata> findColumns() {
        return allColumns
                .values()
                .stream()
                .flatMap(c -> getForeignKey()
                        .stream()
                        .map(ForeignKeyMetadata::getTarget)
                        .map(TableOrViewMetadata::getColumns)
                        .flatMap(Collection::stream))
                .sorted()
                .collect(Collectors.toList());
    }

    @Override
    public Optional<RDBColumnMetadata> getColumn(String name) {
        return ofNullable(name)
                .map(allColumns::get);
    }

    @Override
    public RDBColumnMetadata getPrimaryMetadata() {
        return getColumns()
                .stream()
                .filter(RDBColumnMetadata::isPrimaryKey)
                .findFirst()
                .orElse(null);
    }

    @Override
    public Optional<RDBColumnMetadata> findColumn(String name) {
        return ofNullable(name)
                .map(this::getColumn)
                .filter(Optional::isPresent)
                .orElseGet(() -> findNestColumn(name));
    }

    private Optional<RDBColumnMetadata> findNestColumn(String name) {
        if (name == null) {
            return empty();
        }

        if (name.contains(".")) {
            String[] arr = name.split("[.]");
            if (arr.length == 2) {  //table.name
                return findColumnFromSchema(schema, arr[0], arr[1]);

            } else if (arr.length == 3) { //schema.table.name
                return schema.getDatabase()
                             .getSchema(arr[0])
                             .flatMap(another -> findColumnFromSchema(another, arr[1], arr[2]));
            }
        }
        return empty();
    }

    @Override
    public void addForeignKey(ForeignKeyMetadata metadata) {
        foreignKey.add(metadata);

    }

    @Override
    public ForeignKeyMetadata addForeignKey(ForeignKeyBuilder builder) {
        ForeignKeyMetadata metadata = LazyForeignKeyMetadata.of(builder, this);

        addForeignKey(metadata);
        return metadata;
    }

    private Optional<RDBColumnMetadata> findColumnFromSchema(RDBSchemaMetadata schema, String tableName, String column) {
        return of(schema.getTableOrView(tableName)
                        .flatMap(meta -> meta.getColumn(column)))
                .filter(Optional::isPresent)
                .orElseGet(() -> getForeignKey(tableName) //查找外键关联信息
                                                          .flatMap(key -> key.getTarget().getColumn(column)));
    }

    @Override
    public List<ForeignKeyMetadata> getForeignKeys() {
        return new ArrayList<>(foreignKey);
    }

    @Override
    public Optional<ForeignKeyMetadata> getForeignKey(String targetName) {
        return foreignKey
                .stream()
                .filter(key -> key.getTarget().equalsNameOrAlias(targetName) || key.equalsNameOrAlias(targetName))
                .findFirst();
    }

    public void addFeature(Feature feature) {
        features.put(feature.getId(), feature);
    }

    @Override
    public Dialect getDialect() {
        return getSchema()
                .getDialect();
    }

    @Override
    @SneakyThrows
    public ObjectMetadata clone() {
        return (ObjectMetadata) super.clone();
    }

    public RDBColumnMetadata newColumn() {
        RDBColumnMetadata column = new RDBColumnMetadata();
        column.setSortIndex(getColumns().size() + 1);
        column.setOwner(this);
        return column;
    }

    @Override
    public String toString() {
        return FeatureUtils.metadataToString(this);
    }

    @Override
    public void merge(TableOrViewMetadata metadata) {
        metadata.getForeignKeys().forEach(this::addForeignKey);
        metadata.getFeatureList().forEach(this::addFeature);
        metadata.getColumns().forEach(this::addColumn);

    }

    @Override
    public void replace(TableOrViewMetadata metadata) {
        foreignKey.clear();
        features.clear();
        allColumns.clear();
        merge(metadata);
    }
}
