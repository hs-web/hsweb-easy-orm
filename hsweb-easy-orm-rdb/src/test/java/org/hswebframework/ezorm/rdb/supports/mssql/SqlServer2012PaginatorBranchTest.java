package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.rdb.operator.builder.FragmentBlock;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.BlockSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.junit.Assert;
import org.junit.Test;

public class SqlServer2012PaginatorBranchTest {
    @Test
    public void testBlockAndAppendableBranches() {
        SqlServer2012Paginator paginator = new SqlServer2012Paginator();
        BlockSqlFragments block = BlockSqlFragments.of();
        block.addBlock(FragmentBlock.orderBy, "order by id");
        Assert.assertTrue(paginator.doPaging(block, 2, 10).toRequest().getSql().contains("offset ? rows fetch next ? rows only"));
        Assert.assertTrue(paginator.doPaging(PrepareSqlFragments.of("select * from t"), 1, 5).toRequest().getSql().contains("offset ? rows fetch next ? rows only"));
    }
}
