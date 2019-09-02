package org.hswebframework.ezorm.rdb.operator.dml.query;

import org.hswebframework.ezorm.rdb.meta.RDBFutures;

public interface Selects {

    static SelectColumnOperator column(String name) {
        return new SelectColumnOperator(name);
    }

    static SelectColumnOperator sum(String name) {
        return new SelectColumnOperator(name, RDBFutures.sum.getFunction());
    }

    static SelectColumnOperator count(String name) {
        return new SelectColumnOperator(name, RDBFutures.count.getFunction());
    }

    static SelectColumnOperator max(String name) {
        return new SelectColumnOperator(name, RDBFutures.max.getFunction());
    }

    static SelectColumnOperator min(String name) {
        return new SelectColumnOperator(name, RDBFutures.min.getFunction());
    }


    static SelectColumnOperator avg(String name) {
        return new SelectColumnOperator(name, RDBFutures.avg.getFunction());
    }


}
