package org.hswebframework.ezorm.rdb.supports.kingbaseES.mssql;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.rdb.executor.DefaultBatchSqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.supports.mssql.SqlServerCreateTableSqlBuilder;

/** Keeps native table/index rendering and emits comments as individual statements. */
@Getter
@Setter
public class KingbaseSqlServerCreateTableSqlBuilder extends SqlServerCreateTableSqlBuilder {

    private boolean createComment = true;

    public KingbaseSqlServerCreateTableSqlBuilder() {
        super.setCreateComment(false);
    }

    @Override
    public SqlRequest build(RDBTableMetadata table) {
        DefaultBatchSqlRequest request = (DefaultBatchSqlRequest) super.build(table);
        if (createComment) {
            for (RDBColumnMetadata column : table.getColumns()) {
                if (column.getComment() != null) {
                    request.addBatch(KingbaseSqlServerCommentUtils.column(column));
                }
            }
            if (table.getComment() != null) {
                request.addBatch(KingbaseSqlServerCommentUtils.table(table));
            }
        }
        return request;
    }
}
