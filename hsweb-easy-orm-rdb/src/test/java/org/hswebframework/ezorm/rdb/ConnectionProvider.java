package org.hswebframework.ezorm.rdb;

import java.sql.Connection;

public interface ConnectionProvider {

    Connection getConnection();

    void releaseConnect(Connection connection);
}
