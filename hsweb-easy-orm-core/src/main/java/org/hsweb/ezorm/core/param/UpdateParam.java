package org.hsweb.ezorm.core.param;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Created by zhouhao on 16-4-19.
 */
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
    public UpdateParam clone() {
        UpdateParam param = new UpdateParam();
        param.setData(data);
        param.setExcludes(new LinkedHashSet<>(excludes));
        param.setIncludes(new LinkedHashSet<>(includes));
        List<Term> terms = this.terms.stream().map(term -> term.clone()).collect(Collectors.toList());
        param.setTerms(terms);
        return param;
    }
}
