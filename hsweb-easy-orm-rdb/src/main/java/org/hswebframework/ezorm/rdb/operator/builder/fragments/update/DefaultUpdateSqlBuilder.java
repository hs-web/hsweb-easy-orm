package org.hswebframework.ezorm.rdb.operator.builder.fragments.update;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.apache.commons.collections.CollectionUtils;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.EmptySqlRequest;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.key.ForeignKeyMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.*;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.ForeignKeyTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateColumn;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperatorParameter;

import java.util.*;

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
            return EmptySqlRequest.INSTANCE;
        }
        if (CollectionUtils.isEmpty(parameter.getWhere())) {
            throw new UnsupportedOperationException("unsupported no conditions update");
        }

        BatchSqlFragments fragments = new BatchSqlFragments();

        fragments.addSql("update", table.getFullName(), "set");
        int index = 0;
        Set<RDBColumnMetadata> distinctColumns = new HashSet<>();

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
                        columnFragments = new BatchSqlFragments()
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
                        columnFragments = SimpleSqlFragments
                            .of(Arrays.asList(columnMetadata.getQuoteName(), "= ?"),
                                Collections.singletonList(columnMetadata.encode(value)));
                    }

                }
            }

            if (columnFragments.isNotEmpty()) {
                if (index++ != 0) {
                    fragments.addSql(",");
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
