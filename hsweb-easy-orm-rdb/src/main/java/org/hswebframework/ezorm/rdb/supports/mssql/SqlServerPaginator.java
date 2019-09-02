package org.hswebframework.ezorm.rdb.supports.mssql;

import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.BlockSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.Paginator;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;

import static org.hswebframework.ezorm.rdb.operator.builder.FragmentBlock.*;

/**
 * column top pageSize *
 * from (column row_number()
 * over(order by sno asc) as rownumber,*
 * from student) temp_row
 * where rownumber>((pageIndex-1)*pageSize);
 */
@Slf4j
public class SqlServerPaginator implements Paginator {
    @Override
    public SqlFragments doPaging(SqlFragments fragments, int pageIndex, int pageSize) {

        if (fragments instanceof BlockSqlFragments) {
            BlockSqlFragments newBlock = BlockSqlFragments.of();

            BlockSqlFragments block = ((BlockSqlFragments) fragments);
            newBlock.addBlock(before, "select top " + pageSize + " * from (");

            block.getBlock(selectColumn)
                    .add(SqlFragments.single(", row_number() over(order by sno asc) as rownumber"));

            newBlock.addBlock(other, block);

            newBlock.addBlock(after, PrepareSqlFragments.of().addSql(") _row where _row.rownumber > ?")
                    .addParameter(pageIndex * pageSize));

            return newBlock;

        } else {
            log.warn("unsupported sql fragments type [{}] paging ", fragments.getClass());
        }


        return fragments;
    }
}
