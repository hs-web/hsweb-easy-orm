package org.hswebframework.ezorm.rdb.executor.jdbc;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.codec.LongCharSequence;
import org.hswebframework.ezorm.rdb.executor.JdbcParameterBinder;
import org.hswebframework.ezorm.rdb.executor.NullValue;

import java.io.ByteArrayInputStream;
import java.io.StringReader;
import java.sql.*;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class JdbcSqlExecutorHelper {


    @SneakyThrows
    public static List<String> getResultColumns(ResultSetMetaData metaData) {

        int count = metaData.getColumnCount();
        //获取到执行sql后返回的列信息
        List<String> columns = new ArrayList<>(count);
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
            } else if (object instanceof JdbcParameterBinder binder) {
                binder.bind(statement, index++);
            } else if (object instanceof NullValue nullValue) {
                statement.setNull(index++, nullValue.getDataType().getSqlType().getVendorTypeNumber());
            } else if (object instanceof Date) {
                statement.setTimestamp(index++, new java.sql.Timestamp(((Date) object).getTime()));
            } else if (object instanceof byte[] b) {
                statement.setBlob(index++, new ByteArrayInputStream(b));
            } else if (object instanceof LongCharSequence cb) {
                statement.setCharacterStream(index++, cb.reader());
            } else {
                statement.setObject(index++, object);
            }
        }
    }


}
