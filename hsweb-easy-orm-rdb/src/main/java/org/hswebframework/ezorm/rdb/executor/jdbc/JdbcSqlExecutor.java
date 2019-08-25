package org.hswebframework.ezorm.rdb.executor.jdbc;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.List;

import static org.hswebframework.ezorm.rdb.executor.jdbc.JdbcSqlExecutorHelper.*;

public abstract class JdbcSqlExecutor {

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
        PreparedStatement statement = connection.prepareStatement(request.getSql());
        try {
            preparedStatementParameter(statement, request.getParameters());

            int num = statement.executeUpdate();
            // TODO: 2019-08-21 批量sql
            return num;
        } finally {
            releaseStatement(statement);
        }
    }

    @SneakyThrows
    protected void doExecute(Connection connection, SqlRequest request) {
        PreparedStatement statement = connection.prepareStatement(request.getSql());
        try {
            preparedStatementParameter(statement, request.getParameters());

            statement.execute();
        } finally {
            releaseStatement(statement);
        }
    }


    @SneakyThrows
    public <T, R> R doSelect(Connection connection, SqlRequest request, ResultWrapper<T, R> wrapper) {
        PreparedStatement statement = connection.prepareStatement(request.getSql());
        try {
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
                    wrapper.wrapColumn(new JdbcColumnWrapperContext<>(index, i, columns.get(i), value, data));
                }
                index++;
                if (!wrapper.completedWrapRow(index, data)) {
                    break;
                }
            }
            wrapper.completedWrap();
            releaseResultSet(resultSet);
            return wrapper.getResult();
        } finally {
            releaseStatement(statement);
        }
    }
}
