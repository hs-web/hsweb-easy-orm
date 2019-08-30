package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import org.hswebframework.ezorm.core.FeatureType;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.meta.ForeignKeyMetadata;
import org.hswebframework.ezorm.rdb.meta.RDBFeatureType;
import org.hswebframework.ezorm.rdb.meta.RDBFutures;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.WhereFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.ComplexQueryParameter;

import java.util.List;

public class DefaultForeignKeyTermFragmentBuilder implements ForeignKeyTermFragmentBuilder {

    public static final DefaultForeignKeyTermFragmentBuilder INSTANCE = new DefaultForeignKeyTermFragmentBuilder();

    public PrepareSqlFragments createFragments(String tableName, ForeignKeyMetadata key, List<Term> terms) {
        WhereFragmentBuilder builder = RDBFutures.where(key.getTarget());

        ComplexQueryParameter parameter = new ComplexQueryParameter();
        Term term = new Term();
        if (key.getTerms() != null) {
            term.nest().setTerms(key.getTerms());
        }
        term.nest().setTerms(terms);

        parameter.getWhere().add(term);

        // column = ?
        return PrepareSqlFragments.of()
                .addSql("exists(select 1 from ")
                .addSql(key.getTarget().getName())
                .addSql("where")
                .addSql(key.getTargetColumn().getFullName())
                .addSql("=")
                .addSql(key.getSourceColumn().getFullName(tableName))
                .addFragments(builder.createFragments(parameter))
                .addSql(")")
                ;
    }
}
