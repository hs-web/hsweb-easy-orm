package org.hswebframework.ezorm.rdb.supports.mssql;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.DefaultValue;
import org.hswebframework.ezorm.rdb.executor.DefaultBatchSqlRequest;
import org.hswebframework.ezorm.rdb.metadata.AbstractTableOrViewMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CommonAlterTableSqlBuilder;

@Getter
@Setter
@SuppressWarnings("all")
public class SqlServerAlterTableSqlBuilder extends CommonAlterTableSqlBuilder {

    private boolean changeComment = true;

    @Override
    protected void appendDropColumnSql(DefaultBatchSqlRequest batch, RDBColumnMetadata drop) {

        //先删除注释
        batch.addBatch(SqlServerCommentUtils.createDropColumnComment(((RDBTableMetadata) drop.getOwner()), drop).toRequest());
        super.appendDropColumnSql(batch, drop);
    }

    @Override
    protected void appendAddColumnCommentSql(DefaultBatchSqlRequest batch, RDBColumnMetadata column) {
        if (changeComment) {
            batch.addBatch(SqlServerCommentUtils.createDropAdnCreateColumnComment(((RDBTableMetadata) column.getOwner()), column).toRequest());
        }
    }

    @Override
    protected PrepareSqlFragments createAlterColumnFragments(RDBColumnMetadata oldColumn, RDBColumnMetadata newColumn) {
        PrepareSqlFragments fragments = PrepareSqlFragments.of();

        fragments.addSql("alter table", oldColumn.getOwner().getFullName(), "alter column", oldColumn.getQuoteName());

        if (newColumn.getColumnDefinition() != null) {
            fragments.addSql(newColumn.getColumnDefinition());
        } else {
            fragments.addSql(newColumn.getDataType(), newColumn.isNotNull() ? "not null" : "null");
            DefaultValue defaultValue = newColumn.getDefaultValue();

            if (defaultValue instanceof NativeSql) {
                fragments.addSql("default", ((NativeSql) defaultValue).getSql());
            }
        }

        return fragments;
    }


}
