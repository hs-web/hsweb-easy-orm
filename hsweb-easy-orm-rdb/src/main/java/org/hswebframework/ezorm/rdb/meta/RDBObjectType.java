package org.hswebframework.ezorm.rdb.meta;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.hswebframework.ezorm.core.meta.ObjectType;


@Getter
@AllArgsConstructor
public enum RDBObjectType implements ObjectType {
    table("表"),
    column("列"),
    foreign_key("外键"),
    key("键"),
    index("索引"),
    view("视图"),
    function("函数");

    private String name;

    @Override
    public String getId() {
        return name();
    }

}
