package org.hswebframework.ezorm.core.param;

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
    public UpdateParam<T> clone() {
        UpdateParam<T> param = ((UpdateParam) super.clone());
        param.setData(data);
        param.setExcludes(new LinkedHashSet<>(excludes));
        param.setIncludes(new LinkedHashSet<>(includes));
        List<Term> terms = this.terms.stream().map(Term::clone).collect(Collectors.toList());
        param.setTerms(terms);
        return param;
    }
}
