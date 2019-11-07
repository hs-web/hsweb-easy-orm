package org.hswebframework.ezorm.rdb.operator.builder.fragments.update;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.apache.commons.collections.CollectionUtils;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.key.ForeignKeyMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.*;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.ForeignKeyTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateColumn;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperatorParameter;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import static java.util.Optional.*;
import static org.hswebframework.ezorm.rdb.operator.builder.fragments.function.FunctionFragmentBuilder.*;

@AllArgsConstructor(staticName = "of")
@SuppressWarnings("all")
public class DefaultUpdateSqlBuilder extends AbstractTermsFragmentBuilder<UpdateOperatorParameter> implements UpdateSqlBuilder {

    @Getter
    private RDBTableMetadata table;

    @Override
    public SqlRequest build(UpdateOperatorParameter parameter) {

        if (CollectionUtils.isEmpty(parameter.getColumns())) {
            throw new UnsupportedOperationException("no columns are updated");
        }
        if (CollectionUtils.isEmpty(parameter.getWhere())) {
            throw new UnsupportedOperationException("unsupported no conditions update");
        }

        PrepareSqlFragments fragments = PrepareSqlFragments.of();

        fragments.addSql("update", table.getFullName(), "set");
        int index = 0;
        for (UpdateColumn column : parameter.getColumns()) {
            SqlFragments columnFragments = table.getColumn(column.getColumn())
                    .filter(RDBColumnMetadata::isUpdatable)
                    .<SqlFragments>map(columnMetadata -> {
                        Object value = column.getValue();
                        if (value == null) {
                            return EmptySqlFragments.INSTANCE;
                        }

                        PrepareSqlFragments sqlFragments = PrepareSqlFragments.of();
                        sqlFragments.addSql(columnMetadata.getQuoteName(), "=");

                        if (column instanceof NativeSql) {
                            return PrepareSqlFragments.of()
                                    .addSql(((NativeSql) column).getSql())
                                    .addParameter(((NativeSql) column).getParameters());
                        }
                        if (value instanceof NativeSql) {
                            return PrepareSqlFragments.of()
                                    .addSql(columnMetadata.getQuoteName(), "=")
                                    .addSql(((NativeSql) column.getValue()).getSql())
                                    .addParameter(((NativeSql) column.getValue()).getParameters());
                        }

                        sqlFragments.addFragments(ofNullable(column.getFunction())
                                .flatMap(function -> columnMetadata.findFeature(createFeatureId(function)))
                                .map(builder -> builder.create(columnMetadata.getName(), columnMetadata, column.getOpts()))
                                .orElseGet(() -> PrepareSqlFragments.of()
                                        .addSql("?")
                                        .addParameter(columnMetadata.encode(value))));

                        return sqlFragments;

                    }).orElse(EmptySqlFragments.INSTANCE);


            if (columnFragments.isNotEmpty()) {
                if (index++ != 0) {
                    fragments.addSql(",");
                }
                fragments.addFragments(columnFragments);

            }
        }
        if (index == 0) {
            throw new UnsupportedOperationException("No columns are updated");
        }
        fragments.addSql("where");

        SqlFragments where = createTermFragments(parameter, parameter.getWhere());

        if (where.isEmpty()) {
            throw new UnsupportedOperationException("Unsupported No Conditions update");
        }

        fragments.addFragments(where);

        return fragments.toRequest();
    }

    @Override
    protected SqlFragments createTermFragments(UpdateOperatorParameter parameter, Term term) {
        String columnName = term.getColumn();
        if (columnName == null) {
            return EmptySqlFragments.INSTANCE;
        }

        if (columnName.contains(".")) {
            String[] arr = columnName.split("[.]");
            if (table.equalsNameOrAlias(arr[0])) {
                columnName = arr[1];
            } else {
                return table.getForeignKey(arr[0])
                        .flatMap(key -> key.getSource()
                                .findFeature(ForeignKeyTermFragmentBuilder.ID)
                                .map(builder -> builder.createFragments(table.getName(), key, createForeignKeyTerm(key, term))))
                        .orElse(EmptySqlFragments.INSTANCE);
            }
        }

        return table
                .getColumn(columnName)
                .flatMap(column -> column
                        .findFeature(TermFragmentBuilder.createFeatureId(term.getTermType()))
                        .map(termFragment -> termFragment.createFragments(column.getQuoteName(), column, term)))
                .orElse(EmptySqlFragments.INSTANCE);
    }

    protected List<Term> createForeignKeyTerm(ForeignKeyMetadata keyMetadata, Term term) {
        Term copy = term.clone();
        //只要是嵌套到外键表的条件则认为是关联表的条件
        term.setTerms(new LinkedList<>());

        return Collections.singletonList(copy);
    }
}
