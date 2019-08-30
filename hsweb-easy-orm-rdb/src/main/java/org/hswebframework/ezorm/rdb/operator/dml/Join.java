package org.hswebframework.ezorm.rdb.operator.dml;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.param.Term;

import java.util.ArrayList;
import java.util.List;

/**
 * join targetTable alias on ....
 *
 * @see org.hswebframework.ezorm.core.param.SqlTerm
 */
@Getter
@Setter
public class Join {

    private JoinType type;

    private String target;

    private String alias;

    private List<Term> terms = new ArrayList<>();

    public String getAlias() {
        if (alias == null) {
            return target;
        }
        return alias;
    }

    public boolean equalsTargetOrAlias(String name) {
        return getAlias().equals(name) || getTarget().equals(name);
    }
}
