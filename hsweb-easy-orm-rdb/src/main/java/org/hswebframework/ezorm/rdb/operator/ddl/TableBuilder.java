package org.hswebframework.ezorm.rdb.operator.ddl;

import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.ResultOperator;

import java.util.Set;
import java.util.function.Consumer;

/**
 * 数据库表构造器,用于通过DSL方式进行表结构操作.
 *
 * @author zhouhao
 * @since 4.0
 */
public interface TableBuilder {

    /**
     * 添加列
     *
     * @param column 列
     * @return this
     */
    TableBuilder addColumn(RDBColumnMetadata column);

    /**
     * 添加列并返回{@link ColumnBuilder}进行后续列操作,
     * 操作结束后调用{@link ColumnBuilder#commit()}来返回.
     *
     * @return ColumnBuilder
     */
    ColumnBuilder addColumn();

    /**
     * 指定列名添加列并返回{@link ColumnBuilder}进行后续列操作,
     * 操作结束后调用{@link ColumnBuilder#commit()}来返回.
     *
     * @param name 列名 {@link RDBColumnMetadata#getName()}
     * @return ColumnBuilder
     */
    ColumnBuilder addColumn(String name);

    /**
     * 根据列名删除列,此操作不会物理删除数据库表中的列.
     *
     * @param name 列名
     * @return this
     */
    TableBuilder removeColumn(String name);

    /**
     * 根据列名删除列,此操作将会物理删除数据库表中的列.
     *
     * @param name 列名
     * @return TableBuilder
     */
    TableBuilder dropColumn(String name);

    /**
     * 自定义表结构操作
     *
     * @param consumer consumer
     * @return TableBuilder
     */
    TableBuilder custom(Consumer<RDBTableMetadata> consumer);

    /**
     * 设置表注释
     *
     * @param comment 表注释
     * @return TableBuilder
     */
    TableBuilder comment(String comment);

    /**
     * 设置表别名{@link RDBTableMetadata#getAlias()}
     *
     * @param name 表别名
     * @return TableBuilder
     */
    TableBuilder alias(String name);

    /**
     * 设置是否允许修改列
     *
     * @param allow 是否允许
     * @return TableBuilder
     */
    TableBuilder allowAlter(boolean allow);

    /**
     * 设置是否自动加载表结构
     *
     * @param autoLoad 是否自动加载
     * @return TableBuilder
     */
    TableBuilder autoLoad(boolean autoLoad);

    /**
     * 设置是否合并表结构
     *
     * @param merge 是否合并
     * @return TableBuilder
     */
    TableBuilder merge(boolean merge);

    /**
     * 添加索引
     *
     * @return IndexBuilder
     */
    IndexBuilder index();

    /**
     * 添加外键
     *
     * @return ForeignKeyDSLBuilder
     */
    ForeignKeyDSLBuilder foreignKey();

    /**
     * 结束并返回结果操作进行执行
     *
     * @return TableDDLResultOperator
     */
    TableDDLResultOperator commit();


    /**
     * 添加列并通过指定的回调进行列构建
     * <pre>{@code
     *  createOrAlter("test")
     *  .addColumn(column->column.name("id")...)
     *  .commit()
     *  .sync();
     * }</pre>
     *
     * @param consumer 回调
     * @return TableBuilder
     * @since 4.0.14
     */
    default TableBuilder addColumn(Consumer<ColumnBuilder> consumer) {
        ColumnBuilder builder = addColumn();
        consumer.accept(builder);
        return builder.commit();
    }
}
