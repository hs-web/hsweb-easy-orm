package org.hswebframework.ezorm.rdb.supports.postgres;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.utils.SqlUtils;

import java.sql.Connection;

public final class PostgresqlDriverUtils {

    private static final String PG_OBJECT_CLASS = "org.postgresql.util.PGobject";

    private static final String R2DBC_JSON_CLASS = "io.r2dbc.postgresql.codec.Json";

    private static final String R2DBC_VECTOR_CLASS = "io.r2dbc.postgresql.codec.Vector";

    private PostgresqlDriverUtils() {
    }

    @SneakyThrows
    public static String escapeJdbcQuestionOperator(Connection connection, String sql) {
        if (!isPostgresql(connection)) {
            return sql;
        }
        return SqlUtils.escapePostgresqlJdbcQuestionOperator(sql);
    }

    @SneakyThrows
    public static boolean isPostgresql(Connection connection) {
        return connection != null
            && connection.getMetaData() != null
            && connection.getMetaData().getDriverName() != null
            && connection.getMetaData().getDriverName().toLowerCase().contains("postgresql");
    }

    public static boolean isPgObject(Object value) {
        return isExactClass(value, PG_OBJECT_CLASS);
    }

    public static boolean isR2dbcJson(Object value) {
        return isExactClass(value, R2DBC_JSON_CLASS);
    }

    public static boolean isR2dbcVector(Object value) {
        return isExactClass(value, R2DBC_VECTOR_CLASS);
    }

    private static boolean isExactClass(Object value, String className) {
        return value != null && className.equals(value.getClass().getName());
    }
}
