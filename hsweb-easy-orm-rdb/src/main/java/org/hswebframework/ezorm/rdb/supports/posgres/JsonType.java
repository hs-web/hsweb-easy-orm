package org.hswebframework.ezorm.rdb.supports.posgres;

import lombok.Getter;
import org.hswebframework.ezorm.rdb.metadata.DataType;

import java.sql.JDBCType;


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
    public JDBCType getJdbcType() {
        return JDBCType.CLOB;
    }


}
