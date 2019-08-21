package org.hswebframework.ezorm.rdb.executor;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.core.ObjectWrapper;

import java.io.ByteArrayInputStream;
import java.sql.*;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

@Slf4j
public abstract class JdbcSyncSqlExecutor implements SyncSqlExecutor {

    public abstract Connection getConnection();

    public abstract void releaseConnection(Connection connection);

    @SneakyThrows
    public void releaseStatement(Statement statement) {
        statement.close();
    }

    @SneakyThrows
    public void releaseResultSet(ResultSet resultSet) {
        resultSet.close();
    }

    protected String sqlParameterToString(Object[] parameters) {
        if (parameters == null) {
            return "";
        }
        StringBuilder builder = new StringBuilder();
        int i = 0;
        for (Object param : parameters) {
            if (i++ != 0)
                builder.append(",");
            builder.append(param);
            builder.append("(");
            builder.append(param == null ? "null" : param.getClass().getSimpleName());
            builder.append(")");
        }
        return builder.toString();
    }

    protected void printSql(SqlRequest sqlRequest) {
        if (log.isDebugEnabled()) {
            log.debug("==>  Preparing: {}", sqlRequest.getSql());
            if (sqlRequest.getParameters() != null && sqlRequest.getParameters().length > 0) {
                log.debug("==> Parameters: {}", sqlParameterToString(sqlRequest.getParameters()));
                if (sqlRequest instanceof SimpleSqlRequest) {
                    log.debug("==>     Native: {}", ((SimpleSqlRequest) sqlRequest).toNativeSql());
                }
            }
        }
    }

    protected void preparedParam(PreparedStatement statement, Object[] parameter) throws SQLException {
        int index = 1;
        //预编译参数
        for (Object object : parameter) {
            if (object == null) {
                statement.setNull(index++, Types.NULL);
            } else if (object instanceof Date)
                statement.setTimestamp(index++, new java.sql.Timestamp(((Date) object).getTime()));
            else if (object instanceof byte[]) {
                statement.setBlob(index++, new ByteArrayInputStream((byte[]) object));
            } else
                statement.setObject(index++, object);
        }
    }

    @Override
    @SneakyThrows
    public int update(SqlRequest request) {
        printSql(request);

        Connection connection = getConnection();
        PreparedStatement statement = connection.prepareStatement(request.getSql());
        try {
            preparedParam(statement, request.getParameters());

            int num = statement.executeUpdate();
            // TODO: 2019-08-21 批量sql
            return num;
        } finally {
            try {
                releaseStatement(statement);
            } finally {
                releaseConnection(connection);
            }
        }
    }

    @Override
    @SneakyThrows
    public boolean execute(SqlRequest request) {
        printSql(request);

        Connection connection = getConnection();
        PreparedStatement statement = connection.prepareStatement(request.getSql());
        try {
            preparedParam(statement, request.getParameters());

            return statement.execute();
        } finally {
            try {
                releaseStatement(statement);
            } finally {
                releaseConnection(connection);
            }
        }
    }

    @Override
    @SneakyThrows
    public <T> List<T> select(SqlRequest request, ObjectWrapper<T> wrapper) {
        List<T> list = new ArrayList<>();

        select(request, wrapper, data -> {
            list.add(data);
            return true;
        });

        return list;
    }

    @Override
    @SneakyThrows
    public <T> int select(SqlRequest request, ObjectWrapper<T> wrapper, Function<T, Boolean> consumer) {
        printSql(request);

        Connection connection = getConnection();
        PreparedStatement statement = connection.prepareStatement(request.getSql());
        try {
            preparedParam(statement, request.getParameters());

            ResultSet resultSet = statement.executeQuery();
            ResultSetMetaData metaData = resultSet.getMetaData();
            int count = metaData.getColumnCount();
            //获取到执行sql后返回的列信息
            List<String> headers = new ArrayList<>();
            for (int i = 1; i <= count; i++) {
                headers.add(metaData.getColumnLabel(i));
            }
            wrapper.setUp(headers);

            int index = 0;
            while (resultSet.next()) {
                //调用包装器,将查询结果包装为对象
                T data = wrapper.newInstance();
                for (int i = 0; i < headers.size(); i++) {
                    Object value = resultSet.getObject(i + 1);
                    wrapper.wrapper(data, index, headers.get(i), value);
                }
                index++;
                if (wrapper.done(data)) {
                    if (!consumer.apply(data)) {
                        break;
                    }
                }
            }
            releaseResultSet(resultSet);
            return index;
        } finally {
            try {
                releaseStatement(statement);
            } finally {
                releaseConnection(connection);
            }
        }
    }

    @Override
    public <T> T selectSingle(SqlRequest request, ObjectWrapper<T> wrapper) {
        AtomicReference<T> reference = new AtomicReference<>();
        select(request, wrapper, data -> {
            reference.set(data);
            return false;
        });
        return reference.get();
    }
}
