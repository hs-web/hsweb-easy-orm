package org.hswebframework.ezorm;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.jdbc.JdbcSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;

import java.sql.Connection;

@AllArgsConstructor
public class TestSyncSqlExecutor extends JdbcSyncSqlExecutor {

    private ConnectionProvider provider;


    @Override
    public Connection getConnection(SqlRequest sqlRequest) {
        return provider.getConnection();
    }

    @Override
    public void releaseConnection(Connection connection, SqlRequest sqlRequest) {
        provider.releaseConnect(connection);
    }
}
