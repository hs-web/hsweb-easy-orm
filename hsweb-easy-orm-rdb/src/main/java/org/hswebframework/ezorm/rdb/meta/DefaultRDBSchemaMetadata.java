package org.hswebframework.ezorm.rdb.meta;

import org.hswebframework.ezorm.core.meta.AbstractSchemaMetadata;
import org.hswebframework.ezorm.core.meta.ObjectMetadata;
import org.hswebframework.ezorm.core.meta.ObjectType;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class DefaultRDBSchemaMetadata extends AbstractSchemaMetadata {

    private List<ObjectType> allObjectType = Arrays.asList(RDBObjectType.table, RDBObjectType.view);

    @Override
    @SuppressWarnings("all")
    public DefaultRDBDatabaseMetadata<DefaultRDBSchemaMetadata> getDatabase() {
        return ((DefaultRDBDatabaseMetadata) super.getDatabase());
    }

    public Optional<RDBTableMetadata> getTable(String name) {
        return getObject(RDBObjectType.table, name);
    }

    public Optional<RDBViewMetadata> getView(String name) {
        return getObject(RDBObjectType.view, name);
    }

    public Optional<TableOrViewMetadata> getTableOrView(String name) {
        return Optional.of(getObject(RDBObjectType.table, name)
                .map(AbstractTableOrViewMetadata.class::cast))
                .filter(Optional::isPresent)
                .orElseGet(() -> getObject(RDBObjectType.view, name)
                        .map(AbstractTableOrViewMetadata.class::cast))
                .map(TableOrViewMetadata.class::cast);
    }

    @Override
    protected <T extends ObjectMetadata> List<T> parseMeta(ObjectType type) {
        return super.<T>parseMeta(type)
                .stream()
                .map(this::metadataParsed)
                .collect(Collectors.toList());
    }


    protected <T extends ObjectMetadata> T metadataParsed(T metadata) {
        if (metadata instanceof AbstractTableOrViewMetadata) {
            ((AbstractTableOrViewMetadata) metadata).setSchema(this);
        }
        return metadata;
    }


    @Override
    protected <T extends ObjectMetadata> T parseMeta(ObjectType type, String name) {
        T metadata = super.parseMeta(type, name);

        return this.metadataParsed(metadata);
    }

    @Override
    public List<ObjectType> getAllObjectType() {
        return allObjectType;
    }
}
