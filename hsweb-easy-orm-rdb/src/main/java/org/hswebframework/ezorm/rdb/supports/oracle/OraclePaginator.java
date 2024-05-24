package org.hswebframework.ezorm.rdb.supports.oracle;

import org.hswebframework.ezorm.rdb.operator.builder.FragmentBlock;
import org.hswebframework.ezorm.rdb.operator.builder.Paginator;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.*;

import static org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments.*;

public class OraclePaginator implements Paginator {

    static final SqlFragments PREFIX = SqlFragments.of("select * from ( SELECT row_.*, rownum rownum_ FROM (");
    static final String SUFFIX_SQL = ") row_ ) where rownum_ <= ?  AND rownum_ > ?";
    static final SqlFragments SUFFIX = SqlFragments.of(SUFFIX_SQL);

    @Override
    public SqlFragments doPaging(SqlFragments fragments, int pageIndex, int pageSize) {
        if (fragments instanceof BlockSqlFragments) {
            BlockSqlFragments block = ((BlockSqlFragments) fragments);
            block.addBlockFirst(FragmentBlock.before, PREFIX);

            block.addBlock(FragmentBlock.after, SimpleSqlFragments
                .of(SUFFIX_SQL, (pageIndex + 1) * pageSize, pageIndex * pageSize));
            return fragments;
        }

        BatchSqlFragments paging = new BatchSqlFragments(3,2);

        paging.add(PREFIX)
              .add(fragments)
              .add(SUFFIX)
              .addParameter((pageIndex + 1) * pageSize, pageIndex * pageSize);
        return paging;
    }
}
