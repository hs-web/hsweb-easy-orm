package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;

public class EmptyTermFragmentBuilder extends AbstractTermFragmentBuilder {

    private final String symbol;

    public EmptyTermFragmentBuilder(String termType, String name, boolean not) {
        super(termType, name);
        symbol = not ? "!=" : "=";
    }

    @Override
    public SqlFragments createFragments(String columnFullName, RDBColumnMetadata column, Term term) {

        // column = ?
        return SqlFragments.of(columnFullName, symbol, "''");
    }
}
