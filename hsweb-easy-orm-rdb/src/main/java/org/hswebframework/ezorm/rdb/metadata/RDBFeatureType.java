package org.hswebframework.ezorm.rdb.metadata;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.hswebframework.ezorm.core.FeatureType;

@Getter
@AllArgsConstructor
public enum RDBFeatureType implements FeatureType {

    /**
     * @see org.hswebframework.ezorm.rdb.metadata.dialect.Dialect
     */
    dialect("数据库方言"),
    /**
     * @see org.hswebframework.ezorm.rdb.operator.builder.fragments.TermFragmentBuilder
     */
    termType("SQL条件"),

    termsType("SQL条件组合"),

    query("查询"),

    paginator("分页器"),

    sqlBuilder("SQL构造器"),

    sqlExecutor("SQL执行器"),

    metadataParser("元数据解析器"),

    /**
     * @see org.hswebframework.ezorm.rdb.operator.builder.fragments.function.FunctionFragmentBuilder
     */
    function("函数"),


    fragment("SQL片段"),

    foreignKeyTerm("外键关联条件"),

    /**
     * @see org.hswebframework.ezorm.core.ValueCodec
     * @see ValueCodecFactory
     */
    codec("编解码器"),

    exceptionTranslation("异常转换")
    ;


    @Override
    public String getId() {
        return name();
    }

    private String name;

    public String getFeatureId(String suffix) {
        return getId().concat(":").concat(suffix);
    }
}
