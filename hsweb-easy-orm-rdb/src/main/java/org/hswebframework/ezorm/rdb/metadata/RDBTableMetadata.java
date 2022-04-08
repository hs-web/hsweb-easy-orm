package org.hswebframework.ezorm.rdb.metadata;

import lombok.Getter;
import lombok.Setter;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.core.CastUtil;
import org.hswebframework.ezorm.core.meta.ObjectType;
import org.hswebframework.ezorm.rdb.metadata.key.ForeignKeyMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.delete.DefaultDeleteSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.insert.BatchInsertSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.update.DefaultUpdateSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.DefaultSaveOrUpdateOperator;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * 数据库表结构元数据定义信息,用于定义表字段,索引等信息.
 *
 * @author zhouhao
 * @see AbstractTableOrViewMetadata
 * @see RDBIndexMetadata
 * @see ConstraintMetadata
 * @since 4.0
 */
@Getter
@Setter
public class RDBTableMetadata extends AbstractTableOrViewMetadata implements Cloneable {

    //表注释
    private String comment;

    //索引
    private List<RDBIndexMetadata> indexes = new ArrayList<>();

    //外键
    private List<ConstraintMetadata> constraints = new ArrayList<>();

    public RDBTableMetadata(String name) {
        this();
        setName(name);
    }

    public RDBTableMetadata() {
        super();
        //默认的增删改查SQL支持
        addFeature(BatchInsertSqlBuilder.of(this));
        addFeature(DefaultUpdateSqlBuilder.of(this));
        addFeature(DefaultDeleteSqlBuilder.of(this));
        addFeature(DefaultSaveOrUpdateOperator.of(this));
    }

    /**
     * 获取外键约束
     *
     * @param name 约束名称
     * @return ConstraintMetadata
     */
    public Optional<ConstraintMetadata> getConstraint(String name) {
        return constraints
                .stream()
                .filter(metadata -> metadata.getName().equalsIgnoreCase(name))
                .findFirst();
    }

    /**
     * 获取索引
     *
     * @param indexName 索引名称
     * @return RDBIndexMetadata
     */
    public Optional<RDBIndexMetadata> getIndex(String indexName) {
        return indexes
                .stream()
                .filter(index -> index.getName().equalsIgnoreCase(indexName))
                .findFirst();
    }

    /**
     * 添加约束
     *
     * @param metadata ConstraintMetadata
     * @see ConstraintMetadata
     */
    public void addConstraint(ConstraintMetadata metadata) {
        Objects.requireNonNull(metadata.getName(), "Constraint name can not be null");
        metadata.setTableName(this.getName());
        constraints.add(metadata);
    }

    /**
     * 添加索引
     *
     * @param index RDBIndexMetadata
     * @see RDBIndexMetadata
     */
    public void addIndex(RDBIndexMetadata index) {
        Objects.requireNonNull(index.getName(), "index name can not be null");
        index.setTableName(getName());

        indexes.add(index);
        for (RDBIndexMetadata.IndexColumn column : index.getColumns()) {
            getColumn(column.getColumn())
                    .ifPresent(columnMeta -> {
                        //获取列并设置为主键
                        if (index.isPrimaryKey()) {
                            columnMeta.setPrimaryKey(true);
                        }
                    });
        }
    }

    @Override
    public ObjectType getObjectType() {
        return RDBObjectType.table;
    }

    @Override
    @SneakyThrows
    public RDBTableMetadata clone() {
        RDBTableMetadata clone = (RDBTableMetadata) super.clone();
        clone.setAllColumns(new ConcurrentHashMap<>());

        this.getColumns()
            .stream()
            .map(RDBColumnMetadata::clone)
            .forEach(clone::addColumn);

        clone.setFeatures(new ConcurrentHashMap<>(getFeatures()));

        clone.setIndexes(getIndexes()
                                 .stream()
                                 .map(RDBIndexMetadata::clone)
                                 .collect(Collectors.toList()));

        this.getForeignKey()
            .stream()
            .map(ForeignKeyMetadata::clone)
            .map(CastUtil::<ForeignKeyMetadata>cast)
            .forEach(clone::addForeignKey);

        return clone;
    }

    @Override
    public void merge(TableOrViewMetadata metadata) {
        super.merge(metadata);
    }
}
