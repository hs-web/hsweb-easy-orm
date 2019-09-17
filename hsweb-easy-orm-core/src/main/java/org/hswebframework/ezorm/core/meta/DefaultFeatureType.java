package org.hswebframework.ezorm.core.meta;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.hswebframework.ezorm.core.FeatureType;

@AllArgsConstructor
@Getter
public enum DefaultFeatureType implements FeatureType {
    metadataParser("元数据解析器")
    ;

    private String name;

    @Override
    public String getId() {
        return name();
    }


}
