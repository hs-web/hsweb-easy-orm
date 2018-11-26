package org.hswebframework.ezorm.core;

import java.io.Serializable;
import java.util.function.Function;

public interface LambdaColumn<T> extends Function<T, Object>, Serializable {

    default String getColumn(){
        return LambdaColumnConvert.convertToColumn(this);
    }
}
