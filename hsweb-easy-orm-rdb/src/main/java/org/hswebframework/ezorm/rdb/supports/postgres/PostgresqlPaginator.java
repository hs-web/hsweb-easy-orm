package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.operator.builder.FragmentBlock;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.BlockSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.Paginator;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;

public class PostgresqlPaginator implements Paginator {
    @Override
    public SqlFragments doPaging(SqlFragments fragments, int pageIndex, int pageSize) {

        if (fragments instanceof PrepareSqlFragments) {
            ((PrepareSqlFragments) fragments).addSql("limit ? offset ?")
                    .addParameter(pageSize, pageIndex * pageSize);
        } else if (fragments instanceof BlockSqlFragments) {
            ((BlockSqlFragments) fragments).addBlock(FragmentBlock.after, PrepareSqlFragments.of()
                    .addSql("limit ? offset ?")
                    .addParameter(pageSize, pageIndex * pageSize));
        }

        return fragments;
    }
}
