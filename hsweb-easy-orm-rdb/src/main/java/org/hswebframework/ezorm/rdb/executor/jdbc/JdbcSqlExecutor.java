package org.hswebframework.ezorm.rdb.executor.jdbc;

import lombok.AllArgsConstructor;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.executor.BatchSqlRequest;
import org.hswebframework.ezorm.rdb.executor.DefaultColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.slf4j.Logger;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.List;

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
    protected int doUpdate(Connection connection, SqlRequest request) {
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

    @SneakyThrows
    protected void doExecute(Connection connection, SqlRequest request) {
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


    @SneakyThrows
    public <T, R> R doSelect(Connection connection, SqlRequest request, ResultWrapper<T, R> wrapper) {
        PreparedStatement statement = connection.prepareStatement(request.getSql());
        try {
            printSql(logger, request);
            preparedStatementParameter(statement, request.getParameters());
            ResultSet resultSet = statement.executeQuery();
            List<String> columns = getResultColumns(resultSet);

            wrapper.beforeWrap(() -> columns);

            int index = 0;
            while (resultSet.next()) {
                //调用包装器,将查询结果包装为对象
                T data = wrapper.newRowInstance();
                for (int i = 0; i < columns.size(); i++) {
                    Object value = resultSet.getObject(i + 1);
                    DefaultColumnWrapperContext<T> context = new DefaultColumnWrapperContext<>(i, columns.get(i), value, data);
                    wrapper.wrapColumn(context);
                    data = context.getRowInstance();
                }
                index++;
                if (!wrapper.completedWrapRow(data)) {
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
}
