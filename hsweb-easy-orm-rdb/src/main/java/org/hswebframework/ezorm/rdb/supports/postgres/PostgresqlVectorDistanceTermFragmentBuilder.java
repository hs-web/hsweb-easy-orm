package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.EmptySqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.TermFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.AbstractTermFragmentBuilder;

import java.util.HashMap;
import java.util.Map;

public class PostgresqlVectorDistanceTermFragmentBuilder extends AbstractTermFragmentBuilder {
    public static final Map<VectorTermType, PostgresqlVectorDistanceTermFragmentBuilder> ALL = new HashMap<>();

    static {
        for (VectorTermType value : VectorTermType.values()) {
            ALL.put(value, new PostgresqlVectorDistanceTermFragmentBuilder(value));
        }
    }

    private final VectorTermType type;

    public PostgresqlVectorDistanceTermFragmentBuilder(VectorTermType type) {
        super(type.name(), "向量距离查询");
        this.type = type;
    }

    @Override
    public SqlFragments createFragments(String columnFullName, RDBColumnMetadata column, Term term) {
        VectorQueryParam vectorTerm = VectorQueryParam.of(term.getValue());
        String vectorColumn = VectorUtils.getVectorDistanceColumn(columnFullName, type, vectorTerm.getVector());
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
