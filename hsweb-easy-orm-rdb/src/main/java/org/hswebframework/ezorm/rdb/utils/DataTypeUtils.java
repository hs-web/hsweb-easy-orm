package org.hswebframework.ezorm.rdb.utils;

import org.hswebframework.ezorm.rdb.metadata.DataType;

import java.sql.JDBCType;
import java.sql.SQLType;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Date;

public class DataTypeUtils {

    static {

    }

    public static boolean typeIsDate(DataType type) {
        if (type == null) {
            return false;
        }

        SQLType sqlType = type.getSqlType();
        Class clazz = type.getJavaType();

        if (sqlType != null) {
            if (sqlType instanceof JDBCType)
                switch (((JDBCType) sqlType)) {
                    case DATE:
                    case TIME:
                    case TIMESTAMP:
                    case TIME_WITH_TIMEZONE:
                    case TIMESTAMP_WITH_TIMEZONE:
                        return true;
                }
        }

        if (clazz != null) {

            return (Date.class.isAssignableFrom(clazz) ||
                    clazz == LocalDate.class ||
                    clazz == LocalTime.class ||
                    clazz == LocalDateTime.class ||
                    clazz == Instant.class
            );
        }


        return false;

    }

    public static boolean sqlTypeIsNumber(SQLType sqlType) {
        if (sqlType != null) {
            if (sqlType instanceof JDBCType)
                switch (((JDBCType) sqlType)) {
                    case NUMERIC:
                    case DECIMAL:
                    case BIT:
                    case BIGINT:
                    case INTEGER:
                    case TINYINT:
                    case FLOAT:
                    case DOUBLE:
                        return true;
                }
        }
        return false;
    }

    public static boolean typeIsNumber(DataType type) {
        if (type == null) {
            return false;
        }

        if (sqlTypeIsNumber(type.getSqlType())) {
            return true;
        }

        if (type.getJavaType() != null) {
            Class<?> clazz = type.getJavaType();

            if (Number.class.isAssignableFrom(clazz)) {
                return true;
            }
            switch (clazz.getSimpleName()) {
                case "byte":
                case "short":
                case "int":
                case "float":
                case "double":
                case "long":
                    return true;
            }
        }
        return false;

    }

}
