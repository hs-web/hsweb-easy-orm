package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.rdb.operator.builder.FragmentBlock;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.BlockSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.Paginator;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;

import java.util.LinkedList;

import static org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments.*;

public class SqlServer2012Paginator implements Paginator {
    @Override
    public SqlFragments doPaging(SqlFragments fragments, int pageIndex, int pageSize) {

        if (fragments instanceof BlockSqlFragments) {
            BlockSqlFragments block = ((BlockSqlFragments) fragments);
            LinkedList<SqlFragments> orderBy = block.getBlock(FragmentBlock.orderBy);
            if (orderBy.isEmpty()) {
                orderBy.add(SqlFragments.single("order by 1"));
            }
            block.addBlock(FragmentBlock.after, of("offset ? rows fetch next ? rows only", pageIndex * pageSize, pageSize));

            return block;
        } else if (fragments instanceof PrepareSqlFragments) {
            PrepareSqlFragments sqlFragments = ((PrepareSqlFragments) fragments);
            if (!sqlFragments.getSql().contains("order by")
                    && !sqlFragments.getSql().contains("ORDER BY")) {
                sqlFragments.addSql("order", "by", "1");
            }
            sqlFragments.addSql("offset ? rows fetch next ? rows only")
                    .addParameter(pageIndex * pageSize, pageSize);
            return sqlFragments;
        }
        return fragments;
    }
}
