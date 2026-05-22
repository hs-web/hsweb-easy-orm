package org.hswebframework.ezorm.rdb.supports.postgres;

import io.r2dbc.spi.Parameters;
import io.r2dbc.spi.Statement;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.JdbcParameterBinder;
import org.hswebframework.ezorm.rdb.executor.R2dbcParameterBinder;

import java.lang.reflect.Array;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Types;
import java.util.StringJoiner;

@Getter
@RequiredArgsConstructor(staticName = "of")
class PostgresqlArrayParameter implements JdbcParameterBinder, R2dbcParameterBinder {

    private final PostgresqlArrayType type;

    private final Object value;

    @Override
    public void bind(PreparedStatement statement, int index) throws SQLException {
        if (value == null) {
            statement.setNull(index, Types.ARRAY);
            return;
        }
        statement.setArray(index, statement.getConnection().createArrayOf(type.getJdbcElementType(), toObjectArray()));
    }

    @Override
    public void bind(Statement statement, String identifier) {
        if (value == null) {
            statement.bind(identifier, Parameters.in(type.getR2dbcArrayType()));
            return;
        }
        statement.bind(identifier, Parameters.in(type.getR2dbcArrayType(), value));
    }

    private Object[] toObjectArray() {
        if (value instanceof Object[] values) {
            return values;
        }
        int len = Array.getLength(value);
        Object[] values = new Object[len];
        for (int i = 0; i < len; i++) {
            values[i] = Array.get(value, i);
        }
        return values;
    }

    @Override
    public String toString() {
        if (value == null) {
            return "null::" + type.getId();
        }
        StringJoiner joiner = new StringJoiner(",", "{", "}::" + type.getId());
        for (Object element : toObjectArray()) {
            joiner.add(element == null ? "NULL" : String.valueOf(element));
        }
        return joiner.toString();
    }
}
