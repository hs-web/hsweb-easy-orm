package org.hswebframework.ezorm.rdb.supports.posgres;

import org.hswebframework.ezorm.rdb.operator.builder.FragmentBlock;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.BlockSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.Paginator;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;

public class PosgresPaginator implements Paginator {
    @Override
    public SqlFragments doPaging(SqlFragments fragments, int limit, int offset) {

        if (fragments instanceof PrepareSqlFragments) {
            ((PrepareSqlFragments) fragments).addSql("paging ? offset ?")
                    .addParameter(limit, offset);
        } else if (fragments instanceof BlockSqlFragments) {
            ((BlockSqlFragments) fragments).addBlock(FragmentBlock.after, PrepareSqlFragments.of()
                    .addSql("paging ? offset ?")
                    .addParameter(limit, offset));
        }

        return fragments;
    }
}
