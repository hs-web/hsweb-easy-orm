package org.hswebframework.ezorm.core;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CastUtil {

    @SuppressWarnings("all")
    public static  <T> T cast(Object value){
        return ((T) value);
    }

}
