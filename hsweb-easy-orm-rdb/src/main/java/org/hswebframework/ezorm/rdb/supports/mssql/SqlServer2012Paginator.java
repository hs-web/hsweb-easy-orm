package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.rdb.operator.builder.FragmentBlock;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.*;
import org.hswebframework.ezorm.rdb.operator.builder.Paginator;

import java.util.Arrays;
import java.util.LinkedList;

import static org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments.*;

public class SqlServer2012Paginator implements Paginator {
    static final SqlFragments ORDER_BY_NULL = SqlFragments.of("order by", "(select null)");

    static final SqlFragments OFFSET = SqlFragments.of("offset ? rows fetch next ? rows only");

    @Override
    public SqlFragments doPaging(SqlFragments fragments, int pageIndex, int pageSize) {

        if (fragments instanceof BlockSqlFragments) {
            BlockSqlFragments block = ((BlockSqlFragments) fragments);
            LinkedList<SqlFragments> orderBy = block.getBlock(FragmentBlock.orderBy);
            if (orderBy.isEmpty()) {
                orderBy.add(ORDER_BY_NULL);
            }
            block.addBlock(FragmentBlock.after, SimpleSqlFragments
                .of(OFFSET.getSql(), Arrays.asList(pageIndex * pageSize, pageSize)));

            return block;
        }
        boolean noOrder = !fragments.getSql().contains("order by")
            && !fragments.getSql().contains("ORDER BY");

        if (!(fragments instanceof AppendableSqlFragments)) {
            fragments = new BatchSqlFragments(3,2).add(fragments);
        }

        AppendableSqlFragments sqlFragments = ((AppendableSqlFragments) fragments);
        if (noOrder) {
            sqlFragments.add(ORDER_BY_NULL);
        }
        sqlFragments
            .add(OFFSET)
            .addParameter(pageIndex * pageSize, pageSize);
        return sqlFragments;
    }
}
