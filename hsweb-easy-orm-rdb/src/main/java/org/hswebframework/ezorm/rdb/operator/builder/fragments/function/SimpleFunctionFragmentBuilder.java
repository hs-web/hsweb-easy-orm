package org.hswebframework.ezorm.rdb.operator.builder.fragments.function;

import lombok.Getter;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.BatchSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.EmptySqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;

import java.util.Map;

@Getter
public class SimpleFunctionFragmentBuilder implements FunctionFragmentBuilder {

    private final String function;

    private final String name;

    private final SqlFragments FUNCTION;

    public SimpleFunctionFragmentBuilder(String function, String name) {
        this.function = function;
        this.name = name;
        FUNCTION = SqlFragments.single(function + "(");
    }


    @Override
    public SqlFragments create(String columnFullName, RDBColumnMetadata metadata, Map<String, Object> opts) {
        if (opts != null) {
            String arg = String.valueOf(opts.get("arg"));
            if ("1".equals(arg)) {
                columnFullName = arg;
            } else if (Boolean.TRUE.equals(opts.get("distinct"))) {
                columnFullName = "distinct " + columnFullName;
            }
        }
        if (columnFullName == null) {
            return EmptySqlFragments.INSTANCE;
        }
        return new BatchSqlFragments(2, 0)
            .add(FUNCTION)
            .addSql(columnFullName, ")");
    }


}
