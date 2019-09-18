package org.hswebframework.ezorm.rdb.executor;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.sql.JDBCType;

@Getter
@AllArgsConstructor(staticName = "of")
public class NullValue {
    private Class type;

    private JDBCType jdbcType;
}
