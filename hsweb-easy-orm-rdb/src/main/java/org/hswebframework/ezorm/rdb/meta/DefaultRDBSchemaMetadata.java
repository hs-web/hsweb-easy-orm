package org.hswebframework.ezorm.rdb.meta;

import org.hswebframework.ezorm.core.meta.AbstractSchemaMetadata;
import org.hswebframework.ezorm.core.meta.ObjectMetadata;
import org.hswebframework.ezorm.core.meta.ObjectType;
import org.hswebframework.ezorm.rdb.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.DefaultForeignKeyTermFragmentBuilder;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class DefaultRDBSchemaMetadata extends AbstractSchemaMetadata {

    private List<ObjectType> allObjectType = Arrays.asList(RDBObjectType.table, RDBObjectType.view);


    public DefaultRDBSchemaMetadata() {
        {
            /* 通用查询条件 */
            registerFeature(RDBFutures.eq);
            registerFeature(RDBFutures.isNull);
            registerFeature(RDBFutures.notNull);
            registerFeature(RDBFutures.not);

            //自动关联外键条件
            registerFeature(DefaultForeignKeyTermFragmentBuilder.INSTANCE);


            /*  函数  */
            registerFeature(RDBFutures.count);
            registerFeature(RDBFutures.sum);
            registerFeature(RDBFutures.max);
            registerFeature(RDBFutures.min);
            registerFeature(RDBFutures.avg);

            // TODO: 2019-08-29 更多设置
        }

    }


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

    public void addTable(RDBTableMetadata metadata) {
        metadata.setSchema(this);
        addObject(metadata);
    }

    public Optional<TableOrViewMetadata> getTableOrView(String name) {
        return Optional.of(getTable(name)
                .map(AbstractTableOrViewMetadata.class::cast))
                .filter(Optional::isPresent)
                .orElseGet(() -> getView(name)
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

    public Dialect getDialect() {
        return getDatabase().getDialect();
    }
}
