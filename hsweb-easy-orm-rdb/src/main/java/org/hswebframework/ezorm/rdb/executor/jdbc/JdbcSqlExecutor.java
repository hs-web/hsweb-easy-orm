package org.hswebframework.ezorm.rdb.executor.jdbc;

import lombok.AllArgsConstructor;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.executor.BatchSqlRequest;
import org.hswebframework.ezorm.rdb.executor.DefaultColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.slf4j.Logger;

import java.sql.*;
import java.util.List;
import java.util.function.Predicate;

import static org.hswebframework.ezorm.rdb.executor.jdbc.JdbcSqlExecutorHelper.*;
import static org.hswebframework.ezorm.rdb.utils.SqlUtils.printSql;

@AllArgsConstructor
public abstract class JdbcSqlExecutor {

    private Logger logger;

    @SneakyThrows
    protected void releaseStatement(Statement statement) {
        statement.close();
    }

    @SneakyThrows
    protected void releaseResultSet(ResultSet resultSet) {
        resultSet.close();
    }

    @SneakyThrows
    protected int doUpdate(Logger logger, Connection connection, SqlRequest request) {
        printSql(logger, request);
        PreparedStatement statement = null;
        try {
            int count = 0;
            if (!request.isEmpty()) {
                statement = connection.prepareStatement(request.getSql());
                preparedStatementParameter(statement, request.getParameters());
                count += statement.executeUpdate();
                logger.debug("==>    Updated: {}", count);
            }

            if (request instanceof BatchSqlRequest) {
                for (SqlRequest batch : ((BatchSqlRequest) request).getBatch()) {
                    if (!batch.isEmpty()) {

                        if (null != statement) {
                            releaseStatement(statement);
                        }
                        printSql(logger, batch);
                        statement = connection.prepareStatement(batch.getSql());
                        preparedStatementParameter(statement, batch.getParameters());
                        int rows = statement.executeUpdate();
                        count += rows;
                        logger.debug("==>    Updated: {}", rows);
                    }
                }
            }
            return count;
        } finally {
            if (null != statement) {
                releaseStatement(statement);
            }
        }
    }

    protected int doUpdate(Connection connection, SqlRequest request) {
        return doUpdate(logger, connection, request);
    }

    @SneakyThrows
    protected void doExecute(Logger logger, Connection connection, SqlRequest request) {
        PreparedStatement statement = null;
        try {
            if (!request.isEmpty()) {
                printSql(logger, request);
                statement = connection.prepareStatement(request.getSql());
                preparedStatementParameter(statement, request.getParameters());
                statement.execute();
            }

            if (request instanceof BatchSqlRequest) {
                for (SqlRequest batch : ((BatchSqlRequest) request).getBatch()) {
                    if (!batch.isEmpty()) {
                        if (null != statement) {
                            releaseStatement(statement);
                        }
                        printSql(logger, batch);
                        statement = connection.prepareStatement(batch.getSql());
                        preparedStatementParameter(statement, batch.getParameters());
                        statement.execute();
                    }
                }
            }
        } finally {
            if (null != statement) {
                releaseStatement(statement);
            }
        }
    }

    protected void doExecute(Connection connection, SqlRequest request) {
        doExecute(logger, connection, request);
    }

    @SneakyThrows
    protected Object getResultValue(ResultSetMetaData metaData, ResultSet set, int columnIndex) {

        switch (metaData.getColumnType(columnIndex)) {
            case Types.TIMESTAMP:
                return set.getTimestamp(columnIndex);
            case Types.TIME:
                return set.getTime(columnIndex);
            case Types.LONGVARCHAR:
            case Types.VARCHAR:
                return set.getString(columnIndex);
            case Types.DATE:
                return set.getDate(columnIndex);
            case Types.CLOB:
                return set.getClob(columnIndex);
            case Types.BLOB:
                return set.getBlob(columnIndex);
            default:
                return set.getObject(columnIndex);
        }
    }

    @SneakyThrows
    protected <T, R> R doSelect(Logger logger,
                                Connection connection,
                                SqlRequest request,
                                ResultWrapper<T, R> wrapper,
                                Predicate<T> stopped) {
        PreparedStatement statement = connection.prepareStatement(request.getSql());
        try {
            printSql(logger, request);
            preparedStatementParameter(statement, request.getParameters());
            ResultSet resultSet = statement.executeQuery();
            ResultSetMetaData metaData = resultSet.getMetaData();
            List<String> columns = getResultColumns(metaData);

            wrapper.beforeWrap(() -> columns);

            int index = 0;
            while (resultSet.next()) {
                //调用包装器,将查询结果包装为对象
                T data = wrapper.newRowInstance();
                for (int i = 0; i < columns.size(); i++) {
                    String column = columns.get(i);
                    Object value = getResultValue(metaData, resultSet, i + 1);
                    DefaultColumnWrapperContext<T> context = new DefaultColumnWrapperContext<>(i, column, value, data);
                    wrapper.wrapColumn(context);
                    data = context.getRowInstance();
                }
                index++;
                if (!wrapper.completedWrapRow(data) || stopped.test(data)) {
                    break;
                }
            }
            wrapper.completedWrap();
            logger.debug("==>    Results: {}", index);
            releaseResultSet(resultSet);
            return wrapper.getResult();
        } finally {
            releaseStatement(statement);
        }
    }


    @SneakyThrows
    public <T, R> R doSelect(Connection connection, SqlRequest request, ResultWrapper<T, R> wrapper) {
        return doSelect(logger, connection, request, wrapper, (t) -> false);
    }
}
