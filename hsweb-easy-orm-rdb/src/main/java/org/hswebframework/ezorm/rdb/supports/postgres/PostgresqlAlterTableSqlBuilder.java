package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.core.DefaultValue;
import org.hswebframework.ezorm.rdb.executor.DefaultBatchSqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CommonAlterTableSqlBuilder;

@SuppressWarnings("all")
public class PostgresqlAlterTableSqlBuilder extends CommonAlterTableSqlBuilder {


    @Override
    protected void appendAlterColumnSql(DefaultBatchSqlRequest batch, RDBColumnMetadata oldColumn, RDBColumnMetadata newColumn) {
        PrepareSqlFragments fragments = PrepareSqlFragments.of();

        fragments.addSql("alter table", oldColumn.getOwner().getFullName(), "alter column", oldColumn.getQuoteName());

        if (newColumn.getColumnDefinition() != null) {
            fragments.addSql(newColumn.getColumnDefinition());
        } else {
            fragments.addSql("type", newColumn.getDataType(), "using",
                    newColumn.getQuoteName().concat("::").concat(newColumn.getDataType()));

            DefaultValue defaultValue = newColumn.getDefaultValue();

            if (defaultValue instanceof NativeSql) {
                fragments.addSql("default", ((NativeSql) defaultValue).getSql());
            }
        }

        batch.addBatch(fragments.toRequest());
        if (oldColumn.isNotNull() != newColumn.isNotNull()) {
            batch.addBatch(PrepareSqlFragments.of()
                    .addSql("alter table", newColumn.getOwner().getFullName(),
                            "alter column", newColumn.getQuoteName(), newColumn.isNotNull() ? "set not null" : "drop not null").toRequest());

        }
    }


}
