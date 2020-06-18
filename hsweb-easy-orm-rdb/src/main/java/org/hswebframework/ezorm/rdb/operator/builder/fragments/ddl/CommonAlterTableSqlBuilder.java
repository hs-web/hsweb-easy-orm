package org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl;

import org.hswebframework.ezorm.core.DefaultValue;
import org.hswebframework.ezorm.rdb.executor.DefaultBatchSqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;

import static org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments.*;

@SuppressWarnings("all")
public class CommonAlterTableSqlBuilder implements AlterTableSqlBuilder {

    public static final CommonAlterTableSqlBuilder INSTANCE = new CommonAlterTableSqlBuilder();

    @Override
    public SqlRequest build(AlterRequest parameter) {

        RDBTableMetadata newTable = parameter.getNewTable();
        RDBTableMetadata oldTable = parameter.getOldTable();

        DefaultBatchSqlRequest batch = new DefaultBatchSqlRequest();

        for (RDBColumnMetadata oldColumn : oldTable.getColumns()) {
            RDBColumnMetadata newColumn = newTable.getColumn(oldColumn.getName()).orElse(null);
            if (newColumn == null) {
                if (parameter.isAllowDrop()) {
                    appendDropColumnSql(batch, oldColumn);
                }
                continue;
            }
            if (parameter.isAllowAlter()
                    && !oldColumn.isPrimaryKey()
                    && oldColumn.ddlModifiable(newColumn)) {
                appendAlterColumnSql(batch, oldColumn, newColumn);
            }
            if (newColumn.getComment() != null && !newColumn.getComment().equals(oldColumn.getComment())) {
                appendAddColumnCommentSql(batch, newColumn);
            }
        }

        for (RDBColumnMetadata newColumn : newTable.getColumns()) {
            if (!oldTable.getColumn(newColumn.getName()).isPresent()) {
                appendAddColumnSql(batch, newColumn);

                appendAddColumnCommentSql(batch, newColumn);
            }
        }

        //index
        for (RDBIndexMetadata index : newTable.getIndexes()) {
            if (index.isPrimaryKey()) {
                continue;
            }
            RDBIndexMetadata oldIndex = oldTable.getIndex(index.getName()).orElse(null);
            if (oldIndex == null) {
                //add index
                appendAddIndexSql(batch, newTable, index);
                continue;
            }
            if (index.isChanged(oldIndex)) {
                appendDropIndexSql(batch, newTable, index);
                appendAddIndexSql(batch, newTable, index);

            }

        }
        return batch;
    }

    protected void appendDropIndexSql(DefaultBatchSqlRequest batch, RDBTableMetadata table, RDBIndexMetadata index) {
        table.findFeature(DropIndexSqlBuilder.ID)
                .map(builder -> builder.build(CreateIndexParameter.of(table, index)))
                .ifPresent(batch::addBatch);
    }

    protected void appendAddIndexSql(DefaultBatchSqlRequest batch, RDBTableMetadata table, RDBIndexMetadata index) {
        table.findFeature(CreateIndexSqlBuilder.ID)
                .map(builder -> builder.build(CreateIndexParameter.of(table, index)))
                .ifPresent(batch::addBatch);
    }

    protected void appendAddColumnCommentSql(DefaultBatchSqlRequest batch, RDBColumnMetadata column) {
        if (column.getComment() == null || column.getComment().isEmpty()) {
            return;
        }
        batch.addBatch(of()
                .addSql("comment on column", column.getFullName(), "is", "'".concat(column.getComment()).concat("'")).toRequest());
    }

    protected void appendAddColumnSql(DefaultBatchSqlRequest batch, RDBColumnMetadata column) {

        batch.addBatch(createAddColumnFragments(column).toRequest());

    }

    protected PrepareSqlFragments createAddColumnFragments(RDBColumnMetadata column) {
        PrepareSqlFragments fragments = of()
                .addSql("alter", "table", column.getOwner().getFullName(), "add", column.getQuoteName());

        if (column.getColumnDefinition() != null) {
            fragments.addSql(column.getColumnDefinition());
        } else {
            fragments.addSql(column.getDataType());
            DefaultValue defaultValue = column.getDefaultValue();
            if (defaultValue instanceof NativeSql) {
                fragments.addSql("default", ((NativeSql) defaultValue).getSql());
            }
            if (column.isNotNull() || column.isPrimaryKey()) {
                fragments.addSql("not null");
            }
        }
        return fragments;
    }

    protected void appendDropColumnSql(DefaultBatchSqlRequest batch,
                                       RDBColumnMetadata drop) {
        batch.addBatch(SqlRequests.of(String.format("alter table %s drop column %s", drop.getOwner().getFullName(), drop.getQuoteName())));
    }

    protected void appendAlterColumnSql(DefaultBatchSqlRequest batch,
                                        RDBColumnMetadata oldColumn,
                                        RDBColumnMetadata newColumn) {

        batch.addBatch(createAlterColumnFragments(oldColumn, newColumn).toRequest());
    }

    protected PrepareSqlFragments createAlterColumnFragments(RDBColumnMetadata oldColumn,
                                                             RDBColumnMetadata newColumn) {
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
        return fragments;
    }
}
