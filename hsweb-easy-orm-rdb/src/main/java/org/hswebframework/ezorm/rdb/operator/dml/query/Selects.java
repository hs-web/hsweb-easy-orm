package org.hswebframework.ezorm.rdb.operator.dml.query;

import org.hswebframework.ezorm.core.StaticMethodReferenceColumn;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatures;

import java.util.Collections;

public interface Selects {

    static SelectColumnOperator column(String name) {
        return new SelectColumnOperator(name);
    }

    static SelectColumnOperator sum(String name) {
        return new SelectColumnOperator(name, RDBFeatures.sum.getFunction());
    }

    static <T> SelectColumnOperator sum(StaticMethodReferenceColumn<T> name) {
        return sum(name.getColumn());
    }

    static SelectColumnOperator count(String name) {
        return new SelectColumnOperator(name, RDBFeatures.count.getFunction());
    }

    static <T> SelectColumnOperator count(StaticMethodReferenceColumn<T> name) {
        return count(name.getColumn());
    }

    static SelectColumnOperator count1() {
        return new SelectColumnOperator(null, RDBFeatures.count.getFunction(), Collections.singletonMap("arg", "1"));
    }

    static SelectColumnOperator max(String name) {
        return new SelectColumnOperator(name, RDBFeatures.max.getFunction());
    }

    static <T> SelectColumnOperator max(StaticMethodReferenceColumn<T> name) {
        return max(name.getColumn());
    }


    static SelectColumnOperator min(String name) {
        return new SelectColumnOperator(name, RDBFeatures.min.getFunction());
    }

    static <T> SelectColumnOperator min(StaticMethodReferenceColumn<T> name) {
        return min(name.getColumn());
    }

    static SelectColumnOperator avg(String name) {
        return new SelectColumnOperator(name, RDBFeatures.avg.getFunction());
    }

    static <T> SelectColumnOperator avg(StaticMethodReferenceColumn<T> name) {
        return avg(name.getColumn());
    }



}
