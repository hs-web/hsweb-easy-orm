package org.hswebframework.ezorm.rdb.operator.builder.fragments.delete;

import lombok.AllArgsConstructor;
import org.apache.commons.collections.CollectionUtils;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.ForeignKeyMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.*;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.ForeignKeyTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.delete.DeleteOperatorParameter;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import static org.hswebframework.ezorm.rdb.metadata.RDBFeatureType.foreignKeyTerm;
import static org.hswebframework.ezorm.rdb.metadata.RDBFeatureType.termType;

@SuppressWarnings("all")
@AllArgsConstructor(staticName = "of")
public class DefaultDeleteSqlBuilder extends AbstractTermsFragmentBuilder<DeleteOperatorParameter> implements DeleteSqlBuilder {

    private RDBTableMetadata table;

    @Override
    public SqlRequest build(DeleteOperatorParameter parameter) {
        if (CollectionUtils.isEmpty(parameter.getWhere())) {
            throw new UnsupportedOperationException("Unsupported No Conditions delete");
        }

        PrepareSqlFragments fragments = PrepareSqlFragments.of();
        fragments.addSql("delete from", table.getFullName(), "where");

        SqlFragments where = createTermFragments(parameter, parameter.getWhere());
        if (where.isEmpty()) {
            throw new UnsupportedOperationException("Unsupported No Conditions delete");
        }
        fragments.addFragments(where);

        return fragments.toRequest();
    }

    @Override
    protected SqlFragments createTermFragments(DeleteOperatorParameter parameter, Term term) {
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
                        .flatMap(key -> key.getSourceColumn()
                                .<ForeignKeyTermFragmentBuilder>getFeature(foreignKeyTerm.getId())
                                .map(builder -> builder.createFragments(key.getName(), key, createForeignKeyTerm(key, term))))
                        .orElse(EmptySqlFragments.INSTANCE);
            }
        }

        return table
                .getColumn(columnName)
                .flatMap(column -> column
                        .<TermFragmentBuilder>findFeature(termType.getFeatureId(term.getTermType()))
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
