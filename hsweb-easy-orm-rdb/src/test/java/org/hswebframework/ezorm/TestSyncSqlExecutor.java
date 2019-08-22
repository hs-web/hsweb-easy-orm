package org.hswebframework.ezorm;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.AbstractJdbcSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.JdbcSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;

import java.sql.Connection;
import java.sql.SQLException;

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
