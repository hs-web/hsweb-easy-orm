package org.hswebframework.ezorm.rdb.meta;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.hswebframework.ezorm.core.FeatureType;
import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.TermFragmentBuilder;

@Getter
@AllArgsConstructor
public enum RDBFeatureType implements FeatureType {
    termType("SQL条件", TermFragmentBuilder.class),
    fragment("SQL片段", SqlFragmentBuilder.class)
    ;

    @Override
    public String getId() {
        return name();
    }

    private String text;

    private Class<? extends Feature> typeClass;

    public String getFeatureId(String sortId){
        return getId().concat(":").concat(sortId);
    }
}
