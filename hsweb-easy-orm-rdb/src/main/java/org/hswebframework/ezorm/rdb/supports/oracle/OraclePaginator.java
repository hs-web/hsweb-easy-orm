package org.hswebframework.ezorm.rdb.supports.oracle;

import org.hswebframework.ezorm.rdb.operator.builder.FragmentBlock;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.*;

import static org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments.*;

public class OraclePaginator implements Paginator {

    @Override
    public SqlFragments doPaging(SqlFragments fragments, int pageIndex, int pageSize) {
        if (fragments instanceof PrepareSqlFragments) {
            PrepareSqlFragments paging = of();

            paging.addSql("SELECT * FROM ( SELECT row_.*, rownum rownum_ FROM (")
                    .addFragments(fragments)
                    .addSql(") row_ ) WHERE rownum_ <= ?  AND rownum_ > ?")
                    .addParameter((pageIndex + 1) * pageSize, pageIndex * pageSize);
            return paging;
        } else if (fragments instanceof BlockSqlFragments) {
            BlockSqlFragments block = ((BlockSqlFragments) fragments);
            block.addBlockFirst(FragmentBlock.before, of().addSql("SELECT * FROM ( SELECT row_.*, rownum rownum_ FROM ("));

            block.addBlock(FragmentBlock.after, of().addSql(") row_ ) WHERE rownum_ <= ?  AND rownum_ > ?")
                    .addParameter((pageIndex + 1) * pageSize, pageIndex * pageSize));
        }

        return fragments;
    }
}
