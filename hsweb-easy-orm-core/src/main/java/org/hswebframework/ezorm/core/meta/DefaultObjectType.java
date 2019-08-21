package org.hswebframework.ezorm.core.meta;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum DefaultObjectType implements ObjectType {
    database("数据库"),
    schema("schema")

    ;

    private String name;

    @Override
    public String getType() {
        return name();
    }


}
