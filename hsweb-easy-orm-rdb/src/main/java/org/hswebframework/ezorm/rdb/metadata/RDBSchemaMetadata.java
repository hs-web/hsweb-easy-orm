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

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class RDBSchemaMetadata extends AbstractSchemaMetadata {

    private List<ObjectType> allObjectType = Arrays.asList(RDBObjectType.table, RDBObjectType.view);


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

    public Optional<RDBTableMetadata> getTable(String name) {
        if (name.contains(".")) {
            return findTableOrView(name)
                    .map(RDBTableMetadata.class::cast);
        }
        return getObject(RDBObjectType.table, name);
    }

    public Optional<RDBViewMetadata> getView(String name) {
        return getObject(RDBObjectType.view, name);
    }

    public void addTable(RDBTableMetadata metadata) {
        metadata.setSchema(this);
        addObject(metadata);
    }

    @Override
    public <T extends ObjectMetadata> Optional<T> getObject(ObjectType type, String name) {
        return super.getObject(type, getDialect().clearQuote(name));
    }

    public Optional<TableOrViewMetadata> findTableOrView(String name) {
        return getDatabase().getTableOrView(name);
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
    protected <T extends ObjectMetadata> List<T> loadMetadata(ObjectType type) {
        return super.<T>loadMetadata(type)
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
    protected <T extends ObjectMetadata> T loadMetadata(ObjectType type, String name) {
        T metadata = super.loadMetadata(type, name);

        return this.metadataParsed(metadata);
    }

    public RDBTableMetadata newTable(String name) {
        RDBTableMetadata tableMetadata = new RDBTableMetadata(name);
        tableMetadata.setSchema(this);
        return tableMetadata;
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
