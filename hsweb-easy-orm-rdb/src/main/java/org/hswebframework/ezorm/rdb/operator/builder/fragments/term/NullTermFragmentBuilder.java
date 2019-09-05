package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;

public class NullTermFragmentBuilder extends AbstractTermFragmentBuilder {

    private String symbol;

    public NullTermFragmentBuilder(String termType, String name, boolean isNot) {
        super(termType, name);
        symbol = isNot ? "not" : "is";
    }

    @Override
    public PrepareSqlFragments createFragments(String columnFullName, RDBColumnMetadata column, Term term) {

        // column = ?
        return PrepareSqlFragments.of()
                .addSql(columnFullName, symbol, "null");
    }
}
