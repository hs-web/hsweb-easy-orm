package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.meta.ForeignKeyMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.WhereFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.ComplexQueryParameter;

import java.util.*;

public class DefaultForeignKeyTermFragmentBuilder implements ForeignKeyTermFragmentBuilder {

    public static final DefaultForeignKeyTermFragmentBuilder INSTANCE = new DefaultForeignKeyTermFragmentBuilder();

    public SqlFragments createFragments(String tableName, ForeignKeyMetadata key, List<Term> terms) {
        WhereFragmentBuilder builder = WhereFragmentBuilder.of(key.getTarget(), Collections.emptySet());

        ComplexQueryParameter parameter = new ComplexQueryParameter();
        parameter.setFrom(key.getTarget().getName());

        Term term = new Term();
        if (key.getTerms() != null && !key.getTerms().isEmpty()) {
            term.setTerms(new ArrayList<>(key.getTerms()));
            //嵌套,防止传入or导致任意数据查询问题
            term.nest().setTerms(terms);
        } else {
            term.setTerms(terms);
        }
        parameter.getWhere().add(term);


        PrepareSqlFragments prepareSqlFragments = PrepareSqlFragments.of()
                .addSql("exists(select 1 from")
                .addSql(key.getTarget().getName())
                .addSql("where")
                .addSql(key.getTargetColumn().getFullName())
                .addSql("=")
                .addSql(key.getSourceColumn().getFullName(tableName));

        SqlFragments fragments = builder.createFragments(parameter);
        if (!fragments.isEmpty()) {
            prepareSqlFragments.addSql("and").addFragments(fragments);
        }

        return prepareSqlFragments
                .addSql(")");
    }
}
