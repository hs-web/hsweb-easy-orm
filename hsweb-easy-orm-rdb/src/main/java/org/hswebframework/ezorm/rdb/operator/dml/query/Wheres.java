package org.hswebframework.ezorm.rdb.operator.dml.query;

import org.hswebframework.ezorm.core.param.SqlTerm;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.operator.dml.Operator;

public interface Wheres {

    static Operator<Term> sql(String sql) {
        SqlTerm term = new SqlTerm(sql);

        return () -> term;
    }

}
