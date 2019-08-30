package org.hswebframework.ezorm.rdb.meta;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.hswebframework.ezorm.core.FeatureType;
import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.QuerySqlFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.TermFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.function.FunctionFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.ForeignKeyTermFragmentBuilder;

@Getter
@AllArgsConstructor
public enum RDBFeatureType implements FeatureType {
    termType("SQL条件", TermFragmentBuilder.class),
    function("函数", FunctionFragmentBuilder.class),
    fragment("SQL片段", QuerySqlFragmentBuilder.class),
    foreignKeyTerm("外键关联条件", ForeignKeyTermFragmentBuilder.class);

    @Override
    public String getId() {
        return name();
    }

    private String name;

    private Class<? extends Feature> typeClass;

    public String getFeatureId(String sortId) {
        return getId().concat(":").concat(sortId);
    }
}
