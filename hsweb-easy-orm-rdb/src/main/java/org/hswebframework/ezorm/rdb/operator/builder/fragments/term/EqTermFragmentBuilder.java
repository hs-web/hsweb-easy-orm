package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.meta.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;

public class EqTermFragmentBuilder extends AbstractTermFragmentBuilder {

    public EqTermFragmentBuilder(String termType, String text) {
        super(termType, text);
    }

    @Override
    public PrepareSqlFragments createFragments(String tableName, RDBColumnMetadata column, Term term) {

        // column = ?
        return PrepareSqlFragments.of()
                .addSql(getColumnFullName(tableName, column), "=?")
                .addParameter(convertValue(column, term));
    }
}
