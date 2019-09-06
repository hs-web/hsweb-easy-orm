package org.hswebframework.ezorm.rdb.operator.dml.query;

import org.hswebframework.ezorm.rdb.metadata.RDBFeatures;

public interface Selects {

    static SelectColumnOperator column(String name) {
        return new SelectColumnOperator(name);
    }

    static SelectColumnOperator sum(String name) {
        return new SelectColumnOperator(name, RDBFeatures.sum.getFunction());
    }

    static SelectColumnOperator count(String name) {
        return new SelectColumnOperator(name, RDBFeatures.count.getFunction());
    }

    static SelectColumnOperator max(String name) {
        return new SelectColumnOperator(name, RDBFeatures.max.getFunction());
    }

    static SelectColumnOperator min(String name) {
        return new SelectColumnOperator(name, RDBFeatures.min.getFunction());
    }


    static SelectColumnOperator avg(String name) {
        return new SelectColumnOperator(name, RDBFeatures.avg.getFunction());
    }


}
