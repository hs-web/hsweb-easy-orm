package org.hswebframework.ezorm.rdb.supports.kingbaseES.mssql;

import org.hswebframework.ezorm.rdb.operator.builder.Paginator;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatureType;

import org.hswebframework.ezorm.rdb.operator.builder.FragmentBlock;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.BlockSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.junit.Assert;
import org.junit.Test;

public class KingbaseSqlServerPaginatorTest {
    @Test
    public void testBlockAndAppendableBranches() {
        Paginator paginator = new KingbaseSqlServerSchemaMetadata("dbo").findFeatureNow(RDBFeatureType.paginator.getId());
        BlockSqlFragments block = BlockSqlFragments.of();
        block.addBlock(FragmentBlock.orderBy, "order by id");
        Assert.assertTrue(paginator.doPaging(block, 2, 10).toRequest().getSql().contains("offset ? rows fetch next ? rows only"));
        Assert.assertTrue(paginator.doPaging(PrepareSqlFragments.of("select * from t"), 1, 5).toRequest().getSql().contains("offset ? rows fetch next ? rows only"));
    }

    @Test
    public void testAppendableBranchWithTopLevelOrderBy() {
        Paginator paginator = new KingbaseSqlServerSchemaMetadata("dbo").findFeatureNow(RDBFeatureType.paginator.getId());
        String sql = paginator.doPaging(PrepareSqlFragments.of("select * from t order by id"), 1, 5)
            .toRequest()
            .getSql();
        Assert.assertFalse(sql.contains("order by (select null)"));
        Assert.assertTrue(sql.contains("order by id offset ? rows fetch next ? rows only"));
    }

    @Test
    public void testAppendableBranchWithWindowOrderByOnly() {
        Paginator paginator = new KingbaseSqlServerSchemaMetadata("dbo").findFeatureNow(RDBFeatureType.paginator.getId());
        String sql = paginator.doPaging(PrepareSqlFragments.of("select row_number() over(order by id) as rn from t"), 1, 5)
            .toRequest()
            .getSql();
        Assert.assertTrue(sql.contains("order by (select null) offset ? rows fetch next ? rows only"));
    }
}
