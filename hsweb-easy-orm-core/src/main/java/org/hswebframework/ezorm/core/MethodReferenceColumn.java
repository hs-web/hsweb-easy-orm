package org.hswebframework.ezorm.core;

import java.io.Serializable;
import java.util.function.Supplier;

/**
 * 使用java8的方法引用来定义列以及值,如:
 * <pre>
 *     public UserEntity queryByUserEntity(UserEntity user){
 *     //where name =? and age > ?
 *         return createQuery()
 *         .is(user::getName)
 *         .gt(user::getAge)
 *         .single();
 *     }
 * </pre>
 *
 * @param <T>
 * @see Conditional
 * @see NestConditional
 * @see StaticMethodReferenceColumn
 * @see MethodReferenceConvert
 */
public interface MethodReferenceColumn<T> extends Supplier<T>, Serializable {

    default String getColumn() {
        return MethodReferenceConvert.convertToColumn(this);
    }
}
