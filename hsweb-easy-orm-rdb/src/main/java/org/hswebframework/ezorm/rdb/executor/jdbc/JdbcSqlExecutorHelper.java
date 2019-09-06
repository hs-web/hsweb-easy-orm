package org.hswebframework.ezorm.rdb.executor.jdbc;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.executor.BatchSqlRequest;
import org.hswebframework.ezorm.rdb.executor.PrepareSqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.slf4j.Logger;

import java.io.ByteArrayInputStream;
import java.sql.*;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class JdbcSqlExecutorHelper {

    public static String sqlParameterToString(Object[] parameters) {
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

    public static void printSql(Logger log, SqlRequest sqlRequest) {
        if (log.isDebugEnabled()) {
            if (sqlRequest.isNotEmpty()) {
                log.debug("==>  Preparing: {}", sqlRequest.getSql());
                if (sqlRequest.getParameters() != null && sqlRequest.getParameters().length > 0) {
                    log.debug("==> Parameters: {}", sqlParameterToString(sqlRequest.getParameters()));
                    if (sqlRequest instanceof PrepareSqlRequest) {
                        log.debug("==>     Native: {}", ((PrepareSqlRequest) sqlRequest).toNativeSql());
                    }
                }
            }
        }
    }

    @SneakyThrows
    public static List<String> getResultColumns(ResultSet resultSet) {
        ResultSetMetaData metaData = resultSet.getMetaData();
        int count = metaData.getColumnCount();
        //获取到执行sql后返回的列信息
        List<String> columns = new ArrayList<>();
        for (int i = 1; i <= count; i++) {
            columns.add(metaData.getColumnLabel(i));
        }

        return columns;
    }


    protected static void preparedStatementParameter(PreparedStatement statement, Object[] parameter) throws SQLException {
        if (parameter == null || parameter.length == 0) {
            return;
        }
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


}
