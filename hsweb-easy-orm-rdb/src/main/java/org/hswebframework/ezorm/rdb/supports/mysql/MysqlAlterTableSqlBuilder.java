package org.hswebframework.ezorm.rdb.supports.mysql;

import org.hswebframework.ezorm.rdb.executor.DefaultBatchSqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CommonAlterTableSqlBuilder;

public class MysqlAlterTableSqlBuilder extends CommonAlterTableSqlBuilder {

    @Override
    protected PrepareSqlFragments createAlterColumnFragments(RDBColumnMetadata oldColumn, RDBColumnMetadata newColumn) {
        return super.createAlterColumnFragments(oldColumn, newColumn)
                .addSql("comment", "'".concat(newColumn.getComment()).concat("'"));
    }

    @Override
    protected PrepareSqlFragments createAddColumnFragments(RDBColumnMetadata column) {
        return super.createAddColumnFragments(column)
                .addSql("comment", "'".concat(column.getComment()).concat("'"));
    }

    @Override
    protected void appendAddColumnCommentSql(DefaultBatchSqlRequest batch, RDBColumnMetadata column) {

    }
}
