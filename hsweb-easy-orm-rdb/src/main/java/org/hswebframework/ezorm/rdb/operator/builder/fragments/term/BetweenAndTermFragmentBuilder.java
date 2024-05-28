package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SimpleSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class BetweenAndTermFragmentBuilder extends AbstractTermFragmentBuilder {

    private final String symbol;

    public BetweenAndTermFragmentBuilder(String termType, String name, boolean isNot) {
        super(termType, name);
        symbol = isNot ? "not between ? and ?" : "between ? and ?";
    }

    @Override
    public SqlFragments createFragments(String columnFullName, RDBColumnMetadata column, Term term) {

        List<Object> val = convertList(column, term);
        if (val.size() != 2) {
            List<Object> values = new ArrayList<>(2);
            if (val.isEmpty()) {
                values.add(null);
                values.add(null);
            } else if (val.size() == 1) {
                values.add(val.get(0));
                values.add(val.get(0));
            } else {
                values.add(val.get(0));
                values.add(val.get(1));
            }
            val = values;
        }

        return SimpleSqlFragments
            .of(Arrays.asList(columnFullName, symbol), val);
    }
}
