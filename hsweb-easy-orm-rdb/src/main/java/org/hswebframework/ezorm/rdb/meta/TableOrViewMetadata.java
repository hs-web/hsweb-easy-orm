package org.hswebframework.ezorm.rdb.meta;

import org.hswebframework.ezorm.core.meta.ObjectMetadata;
import org.hswebframework.ezorm.core.meta.ObjectType;

import java.util.List;
import java.util.Optional;

/**
 * DQL 对象元数据: 表或者视图
 *
 * @since 4.0
 */
public interface TableOrViewMetadata extends ObjectMetadata {
    /**
     * @return 元数据所在schema
     */
    DefaultRDBSchemaMetadata getSchema();

    /**
     * @return 获取当前表或者视图所有列
     */
    List<RDBColumnMetadata> getColumns();

    /**
     * @return 获取所有列, 并且包含关联表对列
     */
    List<RDBColumnMetadata> findColumns();

    /**
     * 获取当前表或者视图对列
     *
     * @param name 列名或者别名
     * @return Optional
     * @see RDBColumnMetadata#getName()
     * @see RDBColumnMetadata#getAlias()
     */
    Optional<RDBColumnMetadata> getColumn(String name);

    /**
     * 查找列,可查找通过外键关联表对列或者其他表对列
     *
     * @param name 列全名或别名,比如: user.name , schema1.user.name
     * @return Optional
     */
    Optional<RDBColumnMetadata> findColumn(String name);

    /**
     * 获取全部外键
     *
     * @return 全部外键集合
     */
    List<ForeignKeyMetadata> getForeignKeys();

    /**
     * 根据关联表获取外键
     *
     * @param targetName 关联表名
     * @return Optional
     */
    Optional<ForeignKeyMetadata> getForeignKey(String targetName);

    @Override
    ObjectType getObjectType();
}
