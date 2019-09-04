package org.hswebframework.ezorm.rdb.operator.dml.query;

import org.hswebframework.ezorm.core.param.SqlTerm;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.operator.dml.Operator;

import java.util.function.Supplier;

public interface Wheres {

    static Supplier<Term> sql(String sql) {
        SqlTerm term = new SqlTerm(sql);

        return () -> term;
    }

}
