package org.hswebframework.ezorm;

import java.sql.Connection;

public interface ConnectionProvider {

    Connection getConnection();

    void releaseConnect(Connection connection);
}
