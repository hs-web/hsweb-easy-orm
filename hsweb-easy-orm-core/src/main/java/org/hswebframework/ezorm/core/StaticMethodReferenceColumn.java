package org.hswebframework.ezorm.core;

import java.io.Serializable;
import java.util.function.Function;

/**
 * 使用静态方法引用来定义列名,如:
 * <pre>
 *     createQuery().where(UserEntity::getName,"name").single();
 * </pre>
 *
 * @param <T>
 * @see Conditional
 * @see NestConditional
 * @see MethodReferenceColumn
 */
public interface StaticMethodReferenceColumn<T> extends Function<T, Object>, Serializable {

    default String getColumn() {
        return MethodReferenceConvert.convertToColumn(this);
    }
}
