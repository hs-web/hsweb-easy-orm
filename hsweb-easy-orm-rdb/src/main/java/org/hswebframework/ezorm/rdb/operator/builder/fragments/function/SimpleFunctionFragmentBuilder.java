package org.hswebframework.ezorm.rdb.operator.builder.fragments.function;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;

import java.util.Map;

@Getter
@AllArgsConstructor
public class SimpleFunctionFragmentBuilder implements FunctionFragmentBuilder {

    private String function;

    private String name;


    @Override
    public SqlFragments create(String columnFullName, RDBColumnMetadata metadata, Map<String, Object> opts) {

        if (opts != null) {
            String arg = String.valueOf(opts.get("arg"));
            if ("1".equals(arg)) {
                columnFullName = arg;
            }
        }
        return PrepareSqlFragments
                .of()
                .addSql(function.concat("(").concat(columnFullName).concat(")"));
    }


}
