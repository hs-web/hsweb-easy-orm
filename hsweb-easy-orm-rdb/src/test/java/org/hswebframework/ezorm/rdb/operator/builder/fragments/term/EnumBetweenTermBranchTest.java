package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.junit.Assert;
import org.junit.Test;

public class EnumBetweenTermBranchTest {
    @Test
    public void testEnumAndBetweenBranches() {
        RDBColumnMetadata column = new H2SchemaMetadata("PUBLIC").newTable("t").newColumn(); column.setName("state");
        Assert.assertEquals("t.state = ?", EnumFragmentBuilder.eq.createFragments("t.state", column, Term.of("state", "eq", new Object[]{1,2,3})).toRequest().getSql());
        Assert.assertEquals("t.state != ?", EnumFragmentBuilder.not.createFragments("t.state", column, Term.of("state", "not", new Object[]{1,2,3})).toRequest().getSql());

        BetweenAndTermFragmentBuilder between = new BetweenAndTermFragmentBuilder("between", "区间", false);
        Assert.assertEquals("t.state between ? and ?", between.createFragments("t.state", column, Term.of("state", "between", new Object[]{})).toRequest().getSql());
        Assert.assertEquals("t.state between ? and ?", between.createFragments("t.state", column, Term.of("state", "between", new Object[]{1})).toRequest().getSql());
        Assert.assertEquals("t.state not between ? and ?", new BetweenAndTermFragmentBuilder("nb", "区间", true).createFragments("t.state", column, Term.of("state", "nb", new Object[]{1,2,3})).toRequest().getSql());
    }
}
