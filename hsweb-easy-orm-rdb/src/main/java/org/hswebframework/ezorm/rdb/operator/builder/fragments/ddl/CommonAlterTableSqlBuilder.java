package org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl;

import org.hswebframework.ezorm.core.DefaultValue;
import org.hswebframework.ezorm.rdb.executor.DefaultBatchSqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;

import static org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments.*;

public class CommonAlterTableSqlBuilder implements AlterTableSqlBuilder {
    @Override
    public SqlRequest build(AlterRequest parameter) {

        RDBTableMetadata newTable = parameter.getNewTable();
        RDBTableMetadata oldTable = parameter.getOldTable();

        DefaultBatchSqlRequest batch = new DefaultBatchSqlRequest();

        for (RDBColumnMetadata oldColumn : oldTable.getColumns()) {
            RDBColumnMetadata newColumn = newTable.getColumn(oldColumn.getName()).orElse(null);
            if (newColumn == null) {
                if (parameter.isAllowDrop()) {
                    batch.addBatch(createDropColumn(oldColumn));
                }
                continue;
            }
            if (oldColumn.isChanged(newColumn)) {
                batch.addBatch(createAlterColumn(oldColumn, newColumn));
            }
            if (oldColumn.getComment() != null && !oldColumn.getComment().equals(newColumn.getComment())) {
                batch.addBatch(createCommentColumn(newColumn));
            }
        }

        for (RDBColumnMetadata newColumn : newTable.getColumns()) {
            if (!oldTable.getColumn(newColumn.getName()).isPresent()) {
                batch.addBatch(createAddColumn(newColumn));
                batch.addBatch(createCommentColumn(newColumn));
            }
        }
        return batch;
    }

    protected SqlRequest createCommentColumn(RDBColumnMetadata column) {
        return of()
                .addSql("comment on column", column.getFullName(), "is", "'".concat(column.getComment()).concat("'"))
                .toRequest();
    }

    protected SqlRequest createAddColumn(RDBColumnMetadata column) {
        PrepareSqlFragments fragments = of()
                .addSql("alter", "table", column.getOwner().getFullName(), "add", column.getQuoteName());

        if (column.getColumnDefinition() != null) {
            fragments.addSql(column.getColumnDefinition());
        } else {
            fragments.addSql(column.getDataType());
            if (column.isNotNull()) {
                fragments.addSql("not null");
            }
            DefaultValue defaultValue = column.getDefaultValue();
            if (defaultValue instanceof NativeSql) {
                fragments.addSql("default", ((NativeSql) defaultValue).getSql());
            }
        }

        return fragments.toRequest();
    }

    protected SqlRequest createDropColumn(RDBColumnMetadata drop) {
        return SqlRequests.of(String.format("alter table %s drop column %s", drop.getOwner().getFullName(), drop.getQuoteName()));
    }

    protected SqlRequest createAlterColumn(RDBColumnMetadata oldColumn, RDBColumnMetadata newColumn) {
        PrepareSqlFragments fragments = PrepareSqlFragments.of();

        fragments.addSql("alter table", oldColumn.getOwner().getFullName(), "modify", oldColumn.getQuoteName());

        if (newColumn.getColumnDefinition() != null) {
            fragments.addSql(newColumn.getColumnDefinition());
        } else {
            fragments.addSql(newColumn.getDataType(), newColumn.isNotNull() ? "not null" : "null");
            DefaultValue defaultValue = newColumn.getDefaultValue();

            if (defaultValue instanceof NativeSql) {
                fragments.addSql("default", ((NativeSql) defaultValue).getSql());
            }
        }

        return fragments.toRequest();
    }
}
