package org.hswebframework.ezorm.rdb.supports.posgres;

import lombok.Getter;
import org.hswebframework.ezorm.rdb.metadata.DataType;

import java.sql.JDBCType;
import java.sql.SQLType;


@Getter
public class JsonType implements DataType {

    public static JsonType INSTANCE = new JsonType();

    @Override
    public Class getJavaType() {
        return String.class;
    }

    @Override
    public String getId() {
        return "json";
    }

    @Override
    public String getName() {
        return "json";
    }

    @Override
    public SQLType getSqlType() {
        return JDBCType.CLOB;
    }


}
