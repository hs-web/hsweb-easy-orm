package org.hswebframework.ezorm.rdb.supports.mysql;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.BlockSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.junit.Assert;
import org.junit.Test;

public class MysqlPaginatorBranchTest {
    @Test
    public void testBlockAndAppendableBranches() {
        MysqlPaginator paginator = new MysqlPaginator();
        Assert.assertTrue(paginator.doPaging(BlockSqlFragments.of(), 2, 10).toRequest().getSql().contains("limit"));
        SqlFragments fragments = paginator.doPaging(PrepareSqlFragments.of("select * from t"), 1, 5);
        Assert.assertTrue(fragments.toRequest().getSql().contains("limit"));
        Assert.assertEquals(2, fragments.toRequest().getParameters().length);
    }
}
