package org.hswebframework.ezorm.core.meta;

/**
 * 元数据对象类型,如: 表,视图.
 * 请尽量使用枚举实现此接口.
 *
 * @author zhouhao
 * @see DefaultObjectType
 * @since 4.0.0
 */
public interface ObjectType {

    String getType();

    String getName();

}
