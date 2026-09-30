package org.hswebframework.ezorm.rdb.operator.builder.fragments.function;

import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;

import java.util.Map;

/**
 * 内置 count 函数。调用方明确启用 countRows，且查询不会使非空列因关联变为 NULL 时，
 * 可以将列计数生成为行数计数；其他情况沿用普通 count 的行为。
 */
public class CountFunctionFragmentBuilder extends SimpleFunctionFragmentBuilder {

    public static final String COUNT_ROWS = "countRows";

    public CountFunctionFragmentBuilder() {
        super("count", "计数");
    }

    @Override
    public SqlFragments create(String columnFullName, RDBColumnMetadata metadata, Map<String, Object> opts) {
        if (columnFullName != null
            && metadata != null
            && metadata.isNotNull()
            && opts != null
            && Boolean.TRUE.equals(opts.get(COUNT_ROWS))) {
            return SqlFragments.single("count(*)");
        }
        return super.create(columnFullName, metadata, opts);
    }
}
