package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.operator.builder.FragmentBlock;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.*;
import org.hswebframework.ezorm.rdb.operator.builder.Paginator;

public class PostgresqlPaginator implements Paginator {
    static final String LIMIT = "limit ? offset ?";
    @Override
    public SqlFragments doPaging(SqlFragments fragments, int pageIndex, int pageSize) {

        if (fragments instanceof BlockSqlFragments) {
            return ((BlockSqlFragments) fragments)
                .addBlock(
                    FragmentBlock.after,
                    SimpleSqlFragments
                        .of(LIMIT, pageSize, pageIndex * pageSize));
        }

        if (!(fragments instanceof AppendableSqlFragments)) {
            fragments = new BatchSqlFragments(2,2).add(fragments);
        }

        return ((AppendableSqlFragments) fragments)
            .addSql(LIMIT)
            .addParameter(pageSize, pageIndex * pageSize);
    }
}
