package org.hswebframework.ezorm.core.param;

import lombok.Getter;
import lombok.Setter;

@SuppressWarnings("all")
@Getter
@Setter
public class UpdateParam<T> extends Param {
    private T data;

    public UpdateParam(T data) {
        this.data = data;
    }

    @Override
    public UpdateParam<T> clone() {
        return ((UpdateParam) super.clone());
    }
}
