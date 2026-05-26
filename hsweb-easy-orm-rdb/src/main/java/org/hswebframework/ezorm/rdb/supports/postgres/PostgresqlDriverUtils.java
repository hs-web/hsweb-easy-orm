package org.hswebframework.ezorm.rdb.supports.postgres;

public final class PostgresqlDriverUtils {

    private static final String PG_OBJECT_CLASS = "org.postgresql.util.PGobject";

    private static final String R2DBC_JSON_CLASS = "io.r2dbc.postgresql.codec.Json";

    private static final String R2DBC_VECTOR_CLASS = "io.r2dbc.postgresql.codec.Vector";

    private PostgresqlDriverUtils() {
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
