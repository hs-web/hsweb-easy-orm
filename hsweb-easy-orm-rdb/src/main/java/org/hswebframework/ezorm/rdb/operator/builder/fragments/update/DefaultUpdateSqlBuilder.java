package org.hswebframework.ezorm.rdb.operator.builder.fragments.update;

import com.google.common.collect.Sets;
import lombok.Getter;
import org.apache.commons.collections.CollectionUtils;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.EmptySqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.*;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateColumn;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperatorParameter;

import java.util.*;

import static org.hswebframework.ezorm.rdb.operator.builder.fragments.function.FunctionFragmentBuilder.createFeatureId;

@SuppressWarnings("all")
public class DefaultUpdateSqlBuilder extends AbstractTermsFragmentBuilder<UpdateOperatorParameter> implements UpdateSqlBuilder {

    @Getter
    private RDBTableMetadata table;

    private SqlFragments PREFIX = null;

    private DefaultUpdateSqlBuilder(RDBTableMetadata table) {
        this.table = table;
    }

    public static DefaultUpdateSqlBuilder of(RDBTableMetadata table) {
        return new DefaultUpdateSqlBuilder(table);
    }


    @Override
    public SqlRequest build(UpdateOperatorParameter parameter) {
        if (PREFIX == null) {
            PREFIX = SqlFragments.of("update", table.getFullName(), "set");
        }
        if (CollectionUtils.isEmpty(parameter.getColumns())) {
            return EmptySqlRequest.INSTANCE;
        }
        if (CollectionUtils.isEmpty(parameter.getWhere())) {
            throw new UnsupportedOperationException("unsupported no conditions update");
        }
        int columnSize = parameter.getColumns().size();

        Set<RDBColumnMetadata> distinctColumns = Sets.newHashSetWithExpectedSize(columnSize);

        BatchSqlFragments fragments = new BatchSqlFragments(3 + columnSize, columnSize);

        fragments.add(PREFIX);
        int index = 0;

        for (UpdateColumn column : parameter.getColumns()) {
            SqlFragments columnFragments = EmptySqlFragments.INSTANCE;
            RDBColumnMetadata columnMetadata = table.getColumn(column.getColumn()).orElse(null);
            if (columnMetadata != null && columnMetadata.isUpdatable()) {
                Object value = column.getValue();
                if (value == null) {
                    continue;
                }
                if (column instanceof NativeSql) {
                    columnFragments = SimpleSqlFragments
                        .of(
                            ((NativeSql) column).getSql(),
                            ((NativeSql) column).getParameters()
                        );
                }
                //字段去重
                else if (distinctColumns.add(columnMetadata)) {
                    //函数
                    if (column.getFunction() != null) {
                        columnFragments = new BatchSqlFragments(2, 1)
                            .addSql(columnMetadata.getQuoteName(), "=")
                            .add(
                                columnMetadata
                                    .findFeatureNow(createFeatureId(column.getFunction()))
                                    .create(columnMetadata.getName(), columnMetadata, column)
                            );
                    }
                    //原生SQL
                    else if (value instanceof NativeSql) {
                        columnFragments = SimpleSqlFragments
                            .of(
                                Arrays.asList(columnMetadata.getQuoteName(), "=", ((NativeSql) column.getValue()).getSql()),
                                Arrays.asList(((NativeSql) column.getValue()).getParameters())
                            );
                    }
                    //  = ?
                    else {
                        value = columnMetadata.encode(value);
                        if (value instanceof NativeSql) {
                            columnFragments = SimpleSqlFragments
                                .of(
                                    Arrays.asList(columnMetadata.getQuoteName(), "=", ((NativeSql) column.getValue()).getSql()),
                                    Arrays.asList(((NativeSql) column.getValue()).getParameters())
                                );
                        } else {
                            columnFragments = SimpleSqlFragments
                                .of(Arrays.asList(columnMetadata.getQuoteName(), "= ?"),
                                    Collections.singletonList(value));
                        }
                    }

                }
            }

            if (columnFragments.isNotEmpty()) {
                if (index++ != 0) {
                    fragments.add(SqlFragments.COMMA);
                }
                fragments.add(columnFragments);
            }
        }
        if (index == 0) {
            throw new UnsupportedOperationException("No columns are updated");
        }
        fragments.add(SqlFragments.WHERE);

        SqlFragments where = createTermFragments(parameter, parameter.getWhere());

        if (where.isEmpty()) {
            throw new UnsupportedOperationException("Unsupported No Conditions update");
        }

        fragments.add(where);

        return fragments.toRequest();
    }

    @Override
    protected SqlFragments createTermFragments(UpdateOperatorParameter parameter, Term term) {
        return SimpleTermsFragmentBuilder.createByTable(table, term);
    }

}
