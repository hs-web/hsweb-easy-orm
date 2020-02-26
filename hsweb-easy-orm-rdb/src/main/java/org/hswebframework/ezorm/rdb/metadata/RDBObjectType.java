package org.hswebframework.ezorm.rdb.metadata;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.hswebframework.ezorm.core.meta.ObjectType;


@Getter
@AllArgsConstructor
public enum RDBObjectType implements ObjectType {
    table("表"),
    column("列"),
    foreignKey("外键"),
    constraint("约束"),
    key("键"),
    dataType("数据类型"),
    index("索引"),
    view("视图"),
    function("函数");

    private String name;

    @Override
    public String getId() {
        return name();
    }

}
