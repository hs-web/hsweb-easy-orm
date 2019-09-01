package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.meta.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;

public class EmptyTermFragmentBuilder extends AbstractTermFragmentBuilder {

    private String symbol;

    public EmptyTermFragmentBuilder(String termType, String name, boolean isNot) {
        super(termType, name);
        symbol = isNot ? "=" : "!=";
    }

    @Override
    public PrepareSqlFragments createFragments(String columnFullName, RDBColumnMetadata column, Term term) {

        // column = ?
        return PrepareSqlFragments.of()
                .addSql(columnFullName, symbol, "''");
    }
}
