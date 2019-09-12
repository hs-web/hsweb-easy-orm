package org.hswebframework.ezorm.core.param;

public class UpdateParam<T> extends Param {
    private T data;

    public UpdateParam() {
    }

    public UpdateParam(T data) {
        this.data = data;
    }

    public <C extends UpdateParam<T>> C set(T data) {
        this.data = data;
        return (C) this;
    }

    public T getData() {
        return data;
    }

    public void setData(T data) {
        this.data = data;
    }

    @Override
    public UpdateParam<T> clone() {
        UpdateParam<T> param = ((UpdateParam) super.clone());
        param.setData(data);
        return param;
    }
}
