package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;

public class SymbolTermFragmentBuilder extends AbstractTermFragmentBuilder {

    private final String symbol;

    public SymbolTermFragmentBuilder(String termType, String name, String symbol) {
        super(termType, name);
        this.symbol = symbol;
    }

    @Override
    public SqlFragments createFragments(String columnFullName, RDBColumnMetadata column, Term term) {


        return appendPrepareOrNative(
                PrepareSqlFragments
                        .of()
                        .addSql(columnFullName, symbol),
                convertValue(column, term));
    }
}
