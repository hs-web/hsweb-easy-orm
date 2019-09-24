package org.hswebframework.ezorm.rdb.metadata;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.sql.JDBCType;

@AllArgsConstructor(staticName = "of")
public class JdbcDataType implements DataType {

    @Getter
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
