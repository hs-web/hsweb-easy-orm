package org.hswebframework.ezorm.rdb.operator.builder.fragments.delete;

import lombok.AllArgsConstructor;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections.CollectionUtils;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.key.ForeignKeyMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.*;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.ForeignKeyTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.delete.DeleteOperatorParameter;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

@SuppressWarnings("all")
@RequiredArgsConstructor(staticName = "of")
public class DefaultDeleteSqlBuilder extends AbstractTermsFragmentBuilder<DeleteOperatorParameter> implements DeleteSqlBuilder {

    private final RDBTableMetadata table;

    private SqlFragments DELETE;

    @Override
    public SqlRequest build(DeleteOperatorParameter parameter) {
        if (CollectionUtils.isEmpty(parameter.getWhere())) {
            throw new UnsupportedOperationException("Unsupported No Conditions delete");
        }
        if (DELETE == null) {
            DELETE = SqlFragments.of("delete from", table.getFullName(), "where");
        }

        BatchSqlFragments fragments = new BatchSqlFragments(2, 1);
        fragments.add(DELETE);

        SqlFragments where = createTermFragments(parameter, parameter.getWhere());
        if (where.isEmpty()) {
            throw new UnsupportedOperationException("Unsupported No Conditions delete");
        }
        fragments.addFragments(where);

        return fragments.toRequest();
    }

    @Override
    protected SqlFragments createTermFragments(DeleteOperatorParameter parameter, Term term) {
         return SimpleTermsFragmentBuilder.createByTable(table,term);
    }

}
