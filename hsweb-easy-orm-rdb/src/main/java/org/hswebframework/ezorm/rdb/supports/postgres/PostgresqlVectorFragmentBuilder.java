package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.EmptySqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.TermFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.AbstractTermFragmentBuilder;

public class PostgresqlVectorFragmentBuilder extends AbstractTermFragmentBuilder {

    public static PostgresqlVectorFragmentBuilder vector_l2 = new PostgresqlVectorFragmentBuilder(VectorTermType.vector_l2);
    public static PostgresqlVectorFragmentBuilder vector_cos = new PostgresqlVectorFragmentBuilder(VectorTermType.vector_cos);
    public static PostgresqlVectorFragmentBuilder vector_ip = new PostgresqlVectorFragmentBuilder(VectorTermType.vector_ip);

    private final VectorTermType type;

    PostgresqlVectorFragmentBuilder(VectorTermType type) {
        super(type.name(), "向量查询");
        this.type = type;
    }

    @Override
    public SqlFragments createFragments(String columnFullName, RDBColumnMetadata column, Term term) {
        VectorQueryParam vectorTerm = VectorQueryParam.of(term.getValue());
        String vectorColumn = VectorUtils.getVectorColumn(columnFullName, type, vectorTerm.getVector());
        return createTermFragments(column, vectorColumn, vectorTerm.getTerm(type, term.getColumn()));
    }


    protected SqlFragments createTermFragments(RDBColumnMetadata column,
                                               String vectorColumn,
                                               Term term) {
        TermFragmentBuilder builder = column
            .findFeature(TermFragmentBuilder.createFeatureId(term.getTermType()))
            .orElse(null);

        if (builder != null) {
            return builder
                .createFragments(vectorColumn, column, term);
        }
        return EmptySqlFragments.INSTANCE;
    }


}
