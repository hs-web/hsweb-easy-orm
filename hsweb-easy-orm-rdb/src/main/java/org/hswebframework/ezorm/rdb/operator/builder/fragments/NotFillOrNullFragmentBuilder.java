package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatures;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.AbstractTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.NullTermFragmentBuilder;

/**
 * 判断不等于条件时，添加空值条件.
 * <p>
 * 例如 !=、 not like、 not in等，会将null值过滤掉。需要添加一个 isnull的或条件
 * <p>
 * 示例：
 * <p>
 * 通用sql：    where name != 'a'
 * 填充后sql： where (name isnull or name != 'a')
 *
 * @author zhangji 2023/3/22
 */
public class NotFillOrNullFragmentBuilder extends AbstractTermFragmentBuilder {

    private final NullTermFragmentBuilder     nullBuilder;
    private final AbstractTermFragmentBuilder notBuilder;

    public NotFillOrNullFragmentBuilder(AbstractTermFragmentBuilder notBuilder) {
        super(notBuilder.getTermType(), notBuilder.getName());
        this.notBuilder = notBuilder;
        this.nullBuilder = RDBFeatures.isNull;
    }

    @Override
    public SqlFragments createFragments(String columnFullName,
                                        RDBColumnMetadata column,
                                        Term term) {
        SqlFragments notSqlFragments = notBuilder.createFragments(columnFullName, column, term);
        SqlFragments nullSqlFragments = nullBuilder.createFragments(columnFullName, column, term);

        return PrepareSqlFragments.of()
                .addSql("(")
                .addSql(nullSqlFragments.getSql())
                .addSql("or")
                .addSql(notSqlFragments.getSql())
                .addSql(")")
                .addParameter(notSqlFragments.getParameters());
    }
}
