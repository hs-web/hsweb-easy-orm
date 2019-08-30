package org.hswebframework.ezorm.rdb.operator.dml.query;

import org.hswebframework.ezorm.rdb.meta.RDBFutures;

public interface Selects {

    static SelectOperator select(String name) {
        return new SelectOperator(name);
    }

    static SelectOperator sum(String name) {
        return new SelectOperator(name, RDBFutures.sum.getFunction());
    }

    static SelectOperator count(String name) {
        return new SelectOperator(name, RDBFutures.count.getFunction());
    }

    static SelectOperator max(String name) {
        return new SelectOperator(name, RDBFutures.max.getFunction());
    }



}
