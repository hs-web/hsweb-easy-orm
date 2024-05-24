package org.hswebframework.ezorm.rdb.supports.mysql;

import org.hswebframework.ezorm.rdb.operator.builder.FragmentBlock;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.*;
import org.hswebframework.ezorm.rdb.operator.builder.Paginator;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Collections;

public class MysqlPaginator implements Paginator {
    static final String LIMIT = "limit ?,?";

    @Override
    public SqlFragments doPaging(SqlFragments fragments, int pageIndex, int pageSize) {

        if (fragments instanceof BlockSqlFragments) {
            return ((BlockSqlFragments) fragments)
                .addBlock(
                    FragmentBlock.after,
                    SimpleSqlFragments
                        .of(LIMIT, pageIndex * pageSize, pageSize));
        }

        if (!(fragments instanceof AppendableSqlFragments)) {
            fragments = new BatchSqlFragments().add(fragments);
        }

        return ((AppendableSqlFragments) fragments)
            .addSql(LIMIT)
            .addParameter(pageIndex * pageSize, pageSize);
    }
}
