package org.hswebframework.ezorm.rdb.types;

public enum DataTypes implements DataType {


    //mysql,postgresql
    jsonb,
    json
    ;

    @Override
    public String getId() {
        return name();
    }

    @Override
    public String getName() {
        return name();
    }
}
