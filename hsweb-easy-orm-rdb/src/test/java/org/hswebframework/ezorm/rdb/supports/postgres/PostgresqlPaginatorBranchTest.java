package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.BlockSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.junit.Assert;
import org.junit.Test;

public class PostgresqlPaginatorBranchTest {
    @Test
    public void testBlockAndAppendableBranches() {
        PostgresqlPaginator paginator = new PostgresqlPaginator();
        Assert.assertTrue(paginator.doPaging(BlockSqlFragments.of(), 2, 10).toRequest().getSql().contains("limit ? offset ?"));
        Assert.assertTrue(paginator.doPaging(PrepareSqlFragments.of("select * from t"), 1, 5).toRequest().getSql().contains("limit ? offset ?"));
    }
}
