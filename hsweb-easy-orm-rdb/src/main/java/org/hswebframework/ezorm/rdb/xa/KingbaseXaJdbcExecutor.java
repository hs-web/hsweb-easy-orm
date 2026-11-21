package org.hswebframework.ezorm.rdb.xa;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.jdbc.JdbcSyncSqlExecutor;
import org.springframework.jdbc.datasource.DataSourceUtils;

import javax.sql.DataSource;
import java.sql.Connection;
import java.util.Objects;

/**
 * Opt-in synchronous executor using Spring's transaction-bound JDBC connections.
 * Supply an XA-managed DataSource and invoke inside a JTA transaction boundary.
 * Unlike Framework's default execute(), this executor never suspends that boundary.
 */
public final class KingbaseXaJdbcExecutor extends JdbcSyncSqlExecutor {

    private final DataSource dataSource;

    public KingbaseXaJdbcExecutor(DataSource dataSource) {
        this.dataSource = Objects.requireNonNull(dataSource, "dataSource");
    }

    @Override
    public Connection getConnection(SqlRequest request) {
        return DataSourceUtils.getConnection(dataSource);
    }

    @Override
    public void releaseConnection(Connection connection, SqlRequest request) {
        DataSourceUtils.releaseConnection(connection, dataSource);
    }
}
