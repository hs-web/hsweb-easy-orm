package org.hsweb.ezorm.param;

/**
 * Created by zhouhao on 16-4-19.
 */
public class UpdateParam<T,C extends UpdateParam> extends SqlParam<C> {
    private T data;

    public UpdateParam() {
    }

    public UpdateParam(T data) {
        this.data = data;
    }

    public C set(T data) {
        this.data = data;
        return (C)this;
    }

    public T getData() {
        return data;
    }

    public void setData(T data) {
        this.data = data;
    }

}
