package org.hswebframework.ezorm.rdb.supports.postgres;

import lombok.Getter;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.EmptySqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.function.FunctionFragmentBuilder;

import java.util.Map;

@Getter
public class PostgresqlVectorDistanceFunctionFragmentBuilder implements FunctionFragmentBuilder {

    public static final String function_id = "vector_distance";
    public static final String opt_term_type_key = "termType";
    public static final String opt_vector_value_key = "vectorValue";

    private final String function = function_id;

    private final String name = "向量距离";

    @Override
    public SqlFragments create(String columnFullName, RDBColumnMetadata metadata, Map<String, Object> opts) {
        VectorTermType termType = VectorTermType.of(opts.getOrDefault(opt_term_type_key, VectorTermType.vector_ip));
        Float[] array = VectorType.toFloatArray(opts.get(opt_vector_value_key));
        if (array == null || termType == null) {
            return EmptySqlFragments.INSTANCE;
        }

        String vectorColumn = VectorUtils.getVectorDistanceColumn(columnFullName, termType, array);
        return PrepareSqlFragments.of(vectorColumn);
    }


}
