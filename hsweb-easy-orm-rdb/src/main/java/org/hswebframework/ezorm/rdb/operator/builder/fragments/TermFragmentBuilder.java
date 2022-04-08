package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatureType;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.function.FunctionFragmentBuilder;

/**
 * SQL条件片段构造器,用于将{@link Term}转换为对应的where条件.
 * <p>
 * 实现此接口自定义条件类型{@link Term#getTermType()},实现条件复用.
 *
 * @author zhouhao
 * @see org.hswebframework.ezorm.rdb.operator.builder.fragments.term.AbstractTermFragmentBuilder
 * @see org.hswebframework.ezorm.rdb.operator.builder.fragments.term.LikeTermFragmentBuilder
 * @see org.hswebframework.ezorm.rdb.operator.builder.fragments.term.InTermFragmentBuilder
 * @see org.hswebframework.ezorm.rdb.operator.builder.fragments.term.SymbolTermFragmentBuilder
 * @since 4.0.0
 */
public interface TermFragmentBuilder extends Feature {

    /**
     * 根据termType来创建featureId
     *
     * @param termType termType
     * @return FeatureId
     */
    static FeatureId<TermFragmentBuilder> createFeatureId(String termType) {
        return FeatureId.of(RDBFeatureType.termType.getFeatureId(termType.toLowerCase()));
    }

    @Override
    default String getId() {
        return getType().getFeatureId(getTermType().toLowerCase());
    }

    @Override
    default RDBFeatureType getType() {
        return RDBFeatureType.termType;
    }

    /**
     * @return 条件类型
     * @see Term#getTermType()
     */
    String getTermType();

    /**
     * 创建SQL条件片段
     *
     * @param columnFullName 列全名,如: schema.table
     * @param column         列对应的元数据. {@link Term#getColumn()}
     * @param term           条件.
     * @return SQL片段
     * @see Term
     */
    SqlFragments createFragments(String columnFullName, RDBColumnMetadata column, Term term);

}
