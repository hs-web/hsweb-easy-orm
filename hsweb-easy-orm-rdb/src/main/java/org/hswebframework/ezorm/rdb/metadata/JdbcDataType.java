package org.hswebframework.ezorm.rdb.metadata;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.sql.JDBCType;
import java.sql.SQLType;

@AllArgsConstructor(staticName = "of")
public class JdbcDataType implements DataType {

    @Getter
    private SQLType sqlType;

    @Getter
    private Class<?> javaType;

    @Override
    public String getName() {
        return sqlType.getName().toLowerCase();
    }

    @Override
    public String getId() {
        return sqlType.getName().toLowerCase();
    }


}
