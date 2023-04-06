package org.hswebframework.ezorm.core;

import java.io.Serializable;
import java.util.function.BiConsumer;
import java.util.function.Supplier;

/**
 * 使用java8的方法引用来定义列
 *
 * @param <T>
 * @see Conditional
 * @see NestConditional
 * @see StaticMethodReferenceColumn
 * @see MethodReferenceConverter
 */
public interface SetterMethodReferenceColumn<T, O> extends BiConsumer<T, O>, Serializable {

    default String getColumn() {
        return MethodReferenceConverter.convertToColumn(this);
    }
}
