package org.hswebframework.ezorm.core;

import java.util.Collection;

/**
 * 字段映射器,当一个字段持有映射器时.
 * <ul>
 * <li>
 * 在查询时,会追加一个名为{@link DictionaryCodec#getFieldName()}的字段为{@link DictionaryCodec#decode(Object)} 的值
 * </li>
 * <li>
 * 在修改或者插入时,验证器会首先通过 {@link DictionaryCodec#encode(Object)}来获取一个结果.
 * 如果返回null.则调用{@link DictionaryCodec#decode(Object)} ,并将值放入数据库.
 * 如果继续返回null,则会抛出验证器异常,提示值不再选项范围中
 * </li>
 * </ul>
 *
 * @author zhouhao
 * @see 4.0
 */
public interface DictionaryCodec {

    /**
     * 获取所有选项
     *
     * @return 选项
     */
    Collection<Object> getItems();

    /**
     * 获取转换后的字段名称
     *
     * @return 转换后的字段名称
     */
    String getFieldName();

    /**
     * 将提交的数据,转换为目标数据
     *
     * @param value 提交的数据
     * @return 转换结果
     */
    Object encode(Object value);

    /**
     * 将数据库的数据,转换为目标数据
     *
     * @param data 数据库数据
     * @return 转换结果
     */
    Object decode(Object data);

}
