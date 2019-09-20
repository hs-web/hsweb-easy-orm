package org.hswebframework.ezorm.rdb.supports.mssql;

import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.BlockSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.Paginator;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

import static org.hswebframework.ezorm.rdb.operator.builder.FragmentBlock.*;
import static org.hswebframework.ezorm.rdb.operator.builder.FragmentBlock.orderBy;

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
                    .add(SqlFragments.single(", row_number() over(order by (SELECT 1)) as rownumber"));

            List<SqlFragments> newOrderBy = block.getBlock(orderBy)
                    .stream()
                    .map(frg -> {
                        // TODO: 2019-09-20 不太严谨的做法，将排序指定的表名替换为_row
                        return PrepareSqlFragments.of(frg.getSql()
                                        .stream()
                                        .map(sql -> {
                                            if (sql.contains(".")) {
                                                String[] arr = sql.split("[.]");
                                                arr[0] = "_row";
                                                return String.join(".", arr);
                                            }
                                            return sql;
                                        }).collect(Collectors.toList()),
                                frg.getParameters());
                    }).collect(Collectors.toList());
            block.getBlock(orderBy).clear();
            newBlock.addBlock(other, block);

            newBlock.addBlock(after, PrepareSqlFragments.of().addSql(") _row where _row.rownumber > ?")
                    .addParameter(pageIndex * pageSize));

            newBlock.getBlock(after).addAll(newOrderBy);

            return newBlock;

        } else {
            log.warn("unsupported sql fragments type [{}] paging ", fragments.getClass());
        }


        return fragments;
    }
}
