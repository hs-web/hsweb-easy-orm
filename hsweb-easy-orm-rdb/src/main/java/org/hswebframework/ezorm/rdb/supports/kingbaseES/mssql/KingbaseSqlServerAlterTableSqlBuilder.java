package org.hswebframework.ezorm.rdb.supports.kingbaseES.mssql;

import org.hswebframework.ezorm.rdb.executor.DefaultBatchSqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.supports.mssql.SqlServerAlterTableSqlBuilder;

public class KingbaseSqlServerAlterTableSqlBuilder extends SqlServerAlterTableSqlBuilder {

    @Override
    protected void appendAddColumnCommentSql(DefaultBatchSqlRequest batch, RDBColumnMetadata column) {
        if (isChangeComment() && column.getComment() != null) {
            batch.addBatch(KingbaseSqlServerCommentUtils.column(column));
        }
    }

    @Override
    protected void appendDropColumnSql(DefaultBatchSqlRequest batch, RDBColumnMetadata column) {
        // Kingbase removes column comments with the column itself.
        batch.addBatch(SqlRequests.of("alter table " + column.getOwner().getFullName()
            + " drop column " + column.getQuoteName()));
    }
}
