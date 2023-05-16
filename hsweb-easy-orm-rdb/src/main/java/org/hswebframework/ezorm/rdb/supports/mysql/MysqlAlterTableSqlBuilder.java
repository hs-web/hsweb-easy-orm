package org.hswebframework.ezorm.rdb.supports.mysql;

import org.hswebframework.ezorm.core.utils.StringUtils;
import org.hswebframework.ezorm.rdb.executor.DefaultBatchSqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CommonAlterTableSqlBuilder;

public class MysqlAlterTableSqlBuilder extends CommonAlterTableSqlBuilder {

    @Override
    protected PrepareSqlFragments createAlterColumnFragments(RDBColumnMetadata oldColumn, RDBColumnMetadata newColumn) {

        if (newColumn.getComment() != null) {
            return super.createAlterColumnFragments(oldColumn, newColumn)
                    .addSql("comment",
                            StringUtils.concat("'",newColumn.getComment(),"'"));
        }
        return super.createAlterColumnFragments(oldColumn, newColumn);
    }

    @Override
    protected PrepareSqlFragments createAddColumnFragments(RDBColumnMetadata column) {
        if (column.getComment() != null) {
            return super.createAddColumnFragments(column)
                    .addSql("comment",  StringUtils.concat("'",column.getComment(),"'"));
        }
        return super.createAddColumnFragments(column);
    }

    @Override
    protected void appendAddColumnCommentSql(DefaultBatchSqlRequest batch, RDBColumnMetadata column) {

    }
}
