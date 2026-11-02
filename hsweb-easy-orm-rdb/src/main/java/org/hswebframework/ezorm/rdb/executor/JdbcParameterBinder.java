package org.hswebframework.ezorm.rdb.executor;

import java.sql.PreparedStatement;
import java.sql.SQLException;

public interface JdbcParameterBinder {

    void bind(PreparedStatement statement, int index) throws SQLException;
}
