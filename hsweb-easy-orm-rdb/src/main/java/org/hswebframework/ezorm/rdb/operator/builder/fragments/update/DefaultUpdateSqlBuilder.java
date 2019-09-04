package org.hswebframework.ezorm.rdb.operator.builder.fragments.update;

import lombok.AllArgsConstructor;
import org.apache.commons.collections.CollectionUtils;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.meta.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateColumn;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperatorParameter;

@AllArgsConstructor(staticName = "of")
public class DefaultUpdateSqlBuilder implements UpdateSqlBuilder {

    private RDBTableMetadata table;

    @Override
    public SqlRequest build(UpdateOperatorParameter parameter) {

        if (CollectionUtils.isEmpty(parameter.getColumns())) {
            throw new UnsupportedOperationException("No columns are updated");
        }
        if (CollectionUtils.isEmpty(parameter.getWhere())) {
            throw new UnsupportedOperationException("Unsupported No Conditions update");
        }

        PrepareSqlFragments fragments = PrepareSqlFragments.of();

        fragments.addSql("update")
                .addSql(table.getFullName())
                .addSql("set");

        for (UpdateColumn column : parameter.getColumns()) {
//            table.getColumn(column)
        }

        return fragments.toRequest();
    }
}
