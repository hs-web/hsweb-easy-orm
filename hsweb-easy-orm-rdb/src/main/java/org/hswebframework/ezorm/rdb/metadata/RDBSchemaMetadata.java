package org.hswebframework.ezorm.rdb.metadata;

import org.hswebframework.ezorm.core.meta.AbstractSchemaMetadata;
import org.hswebframework.ezorm.core.meta.ObjectMetadata;
import org.hswebframework.ezorm.core.meta.ObjectType;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.builder.DefaultQuerySqlBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CommonAlterTableSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CommonCreateIndexSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CommonCreateTableSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CommonDropIndexSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.DefaultForeignKeyTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.utils.FeatureUtils;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class RDBSchemaMetadata extends AbstractSchemaMetadata {

    private final List<ObjectType> allObjectType = Arrays.asList(RDBObjectType.table, RDBObjectType.view);


    public RDBSchemaMetadata(String name) {
        {
            //查询
            addFeature(DefaultQuerySqlBuilder.of(this));


            /* 通用查询条件 */
            addFeature(RDBFeatures.eq);
            addFeature(RDBFeatures.not);
            addFeature(RDBFeatures.gt);
            addFeature(RDBFeatures.gte);
            addFeature(RDBFeatures.lt);
            addFeature(RDBFeatures.lte);
            addFeature(RDBFeatures.like);
            addFeature(RDBFeatures.nlike);

            addFeature(RDBFeatures.in);
            addFeature(RDBFeatures.notIn);
            addFeature(RDBFeatures.between);
            addFeature(RDBFeatures.notBetween);

            addFeature(RDBFeatures.eq);
            addFeature(RDBFeatures.isNull);
            addFeature(RDBFeatures.notNull);
            addFeature(RDBFeatures.isEmpty);
            addFeature(RDBFeatures.notEmpty);
            addFeature(RDBFeatures.nEmpty);


            //自动关联外键条件
            addFeature(DefaultForeignKeyTermFragmentBuilder.INSTANCE);


            /*  函数  */
            addFeature(RDBFeatures.count);
            addFeature(RDBFeatures.sum);
            addFeature(RDBFeatures.max);
            addFeature(RDBFeatures.min);
            addFeature(RDBFeatures.avg);


            /* DDL */

            addFeature(CommonCreateTableSqlBuilder.INSTANCE);
            addFeature(CommonAlterTableSqlBuilder.INSTANCE);
            addFeature(CommonCreateIndexSqlBuilder.INSTANCE);
            addFeature(CommonDropIndexSqlBuilder.INSTANCE);

            /* 编解码器工厂 */
            addFeature(DefaultValueCodecFactory.COMMONS);
        }

        this.setName(name);
    }

    @Override
    @SuppressWarnings("all")
    public RDBDatabaseMetadata getDatabase() {
        return ((RDBDatabaseMetadata) super.getDatabase());
    }

    public Optional<RDBTableMetadata> getTable(String name, boolean autoLoad) {
        if (name.contains(".")) {
            return this.getDatabase()
                       .getObject(name, (schema, _name) -> schema.getTable(_name, autoLoad))
                       .map(RDBTableMetadata.class::cast);
        }
        return getObject(RDBObjectType.table, name, autoLoad);
    }

    public Optional<RDBTableMetadata> getTable(String name) {
        return getTable(name,true);
    }

    public Mono<RDBTableMetadata> getTableReactive(String name) {
        return getTableReactive(name, true);
    }

    public Mono<RDBTableMetadata> getTableReactive(String name, boolean autoLoad) {
        if (name.contains(".")) {
            return this
                    .getDatabase()
                    .getObjectReactive(name, (schema, _name) -> schema.getTableReactive(_name, autoLoad));
        }
        return getObjectReactive(RDBObjectType.table, name, autoLoad);
    }

    public Mono<TableOrViewMetadata> getTableOrViewReactive(String name, boolean autoLoad) {
        return getTableReactive(name, autoLoad)
                .cast(TableOrViewMetadata.class)
                .switchIfEmpty(Mono.defer(() -> getViewReactive(name, autoLoad).cast(TableOrViewMetadata.class)));
    }

    public Mono<TableOrViewMetadata> getTableOrViewReactive(String name) {
        return getTableOrViewReactive(name, true);
    }


    public Mono<RDBViewMetadata> getViewReactive(String name) {
        return getObjectReactive(RDBObjectType.view, name);
    }

    public Mono<RDBViewMetadata> getViewReactive(String name, boolean autoLoad) {
        return getObjectReactive(RDBObjectType.view, name, autoLoad);
    }

    public Optional<RDBViewMetadata> getView(String name, boolean autoLoad) {
        return getObject(RDBObjectType.view, name, autoLoad);
    }

    public Optional<RDBViewMetadata> getView(String name) {
        return getView(name, true);
    }

    public void addTable(RDBTableMetadata metadata) {
        metadata.setSchema(this);
        addObject(metadata);
    }

    public Mono<TableOrViewMetadata> findTableOrViewReactive(String name) {

        return getTableOrViewReactive(name, false)
                .switchIfEmpty(getDatabase().getTableOrViewReactive(name));
    }

    public Optional<TableOrViewMetadata> findTableOrView(String name) {
        Optional<TableOrViewMetadata> current = getTableOrView(name, false);
        if (current.isPresent()) {
            return current;
        }
        return getDatabase().getTableOrView(name);
    }

    public Optional<TableOrViewMetadata> getTableOrView(String name, boolean autoLoad) {
        return Optional.of(getTable(name, autoLoad)
                                   .map(AbstractTableOrViewMetadata.class::cast))
                       .filter(Optional::isPresent)
                       .orElseGet(() -> getView(name, autoLoad)
                               .map(AbstractTableOrViewMetadata.class::cast))
                       .map(TableOrViewMetadata.class::cast);
    }

    public Optional<TableOrViewMetadata> getTableOrView(String name) {
        return getTableOrView(name, true);
    }

    @Override
    protected <T extends ObjectMetadata> List<T> loadMetadata(ObjectType type) {
        return super.<T>loadMetadata(type)
                .stream()
                .map(this::metadataParsed)
                .collect(Collectors.toList());
    }

    @Override
    protected <T extends ObjectMetadata> Flux<T> loadMetadataReactive(ObjectType type) {
        return super.<T>loadMetadataReactive(type)
                .map(this::metadataParsed);
    }


    protected <T extends ObjectMetadata> T metadataParsed(T metadata) {
        if (metadata instanceof AbstractTableOrViewMetadata) {
            ((AbstractTableOrViewMetadata) metadata).setSchema(this);
        }
        return metadata;
    }

    @Override
    protected <T extends ObjectMetadata> T loadMetadata(ObjectType type, String name) {
        T metadata = super.loadMetadata(type, name);

        return this.metadataParsed(metadata);
    }

    @Override
    protected <T extends ObjectMetadata> Mono<T> loadMetadataReactive(ObjectType type, String name) {
        return super
                .<T>loadMetadataReactive(type, name)
                .map(this::metadataParsed);
    }

    public RDBTableMetadata newTable(String name) {
        RDBTableMetadata tableMetadata = new RDBTableMetadata(name);
        tableMetadata.setSchema(this);
        return tableMetadata;
    }

    public void loadAllTable() {

        loadMetadata(RDBObjectType.table)
                .forEach(table -> addTable(((RDBTableMetadata) table)));

    }

    public Mono<Void> loadAllTableReactive() {
        return loadMetadataReactive(RDBObjectType.table)
                .doOnNext(table -> addTable(((RDBTableMetadata) table)))
                .then();
    }

    @Override
    public List<ObjectType> getAllObjectType() {
        return allObjectType;
    }

    public Dialect getDialect() {
        return Optional.ofNullable(getDatabase())
                       .map(RDBDatabaseMetadata::getDialect)
                       .orElseGet(() -> this
                               .<Dialect>getFeatures(RDBFeatureType.dialect)
                               .stream()
                               .findFirst()
                               .orElse(null));
    }

    public Optional<TableOrViewMetadata> removeTableOrView(String name) {
        return this.<TableOrViewMetadata>removeObject(RDBObjectType.table, name)
                .map(Optional::of)
                .orElseGet(() -> removeObject(RDBObjectType.view, name));
    }

    @Override
    public RDBSchemaMetadata clone() {
        return (RDBSchemaMetadata) super.clone();
    }

    @Override
    public String toString() {
        return "schema " +
                getName() +
                " (" + getClass().getSimpleName() + ")" +
                "\n" +
                FeatureUtils.featureToString(getFeatureList());

    }
}
