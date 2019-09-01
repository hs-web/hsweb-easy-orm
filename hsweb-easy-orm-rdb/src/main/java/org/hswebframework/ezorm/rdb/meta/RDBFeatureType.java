package org.hswebframework.ezorm.rdb.meta;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.hswebframework.ezorm.core.FeatureType;

@Getter
@AllArgsConstructor
public enum RDBFeatureType implements FeatureType {
    termType("SQL条件"),
    termsType("SQL条件组合"),

    query("查询"),

    paginator("分页器"),

    sqlBuilder("SQL构造器"),

    sqlExecutor("SQL执行器"),


    function("函数"),
    fragment("SQL片段"),
    foreignKeyTerm("外键关联条件");

    @Override
    public String getId() {
        return name();
    }

    private String name;

    public String getFeatureId(String sortId) {
        return getId().concat(":").concat(sortId);
    }
}
