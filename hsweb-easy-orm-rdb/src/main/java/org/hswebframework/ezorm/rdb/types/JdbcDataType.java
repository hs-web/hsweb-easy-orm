package org.hswebframework.ezorm.rdb.types;

import lombok.AllArgsConstructor;

import java.sql.JDBCType;

@AllArgsConstructor(staticName = "of")
public class JdbcDataType implements DataType {

    private JDBCType jdbcType;

    @Override
    public String getName() {
        return jdbcType.name();
    }

    @Override
    public String getId() {
        return jdbcType.name();
    }
}
