package org.hswebframework.ezorm.rdb.supports.h2;

import org.hswebframework.ezorm.rdb.operator.builder.FragmentBlock;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.BlockSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.Paginator;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;

public class H2Paginator implements Paginator {
    @Override
    public SqlFragments doPaging(SqlFragments fragments, int pageIndex, int pageSize) {
        if (fragments instanceof PrepareSqlFragments) {
            ((PrepareSqlFragments) fragments)
                    .addSql("paging ? offset ?")
                    .addParameter(pageSize, pageIndex * pageSize);

        } else if (fragments instanceof BlockSqlFragments) {
            ((BlockSqlFragments) fragments)
                    .addBlock(FragmentBlock.after, PrepareSqlFragments.of()
                    .addSql("paging ? offset ?")
                    .addParameter(pageSize, pageIndex * pageSize));
        }

        return fragments;
    }
}
