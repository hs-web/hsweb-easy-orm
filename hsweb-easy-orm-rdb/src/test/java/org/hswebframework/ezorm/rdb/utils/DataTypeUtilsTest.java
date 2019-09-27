package org.hswebframework.ezorm.rdb.utils;

import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.junit.Test;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.Date;
import java.sql.JDBCType;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

import static org.hswebframework.ezorm.rdb.utils.DataTypeUtils.typeIsDate;
import static org.hswebframework.ezorm.rdb.utils.DataTypeUtils.typeIsNumber;
import static org.junit.Assert.*;
import static org.junit.Assert.assertTrue;

public class DataTypeUtilsTest {

    @Test
    public void testNumberByJavaType() {

        assertFalse(typeIsNumber(null));

        assertTrue(typeIsNumber(DataType.jdbc(JDBCType.OTHER, byte.class)));
        assertTrue(typeIsNumber(DataType.jdbc(JDBCType.OTHER, short.class)));
        assertTrue(typeIsNumber(DataType.jdbc(JDBCType.OTHER, int.class)));
        assertTrue(typeIsNumber(DataType.jdbc(JDBCType.OTHER, float.class)));
        assertTrue(typeIsNumber(DataType.jdbc(JDBCType.OTHER, double.class)));
        assertTrue(typeIsNumber(DataType.jdbc(JDBCType.OTHER, long.class)));

        assertTrue(typeIsNumber(DataType.jdbc(JDBCType.OTHER, Byte.class)));
        assertTrue(typeIsNumber(DataType.jdbc(JDBCType.OTHER, Short.class)));
        assertTrue(typeIsNumber(DataType.jdbc(JDBCType.OTHER, Integer.class)));
        assertTrue(typeIsNumber(DataType.jdbc(JDBCType.OTHER, Float.class)));
        assertTrue(typeIsNumber(DataType.jdbc(JDBCType.OTHER, Double.class)));
        assertTrue(typeIsNumber(DataType.jdbc(JDBCType.OTHER, Long.class)));

        assertTrue(typeIsNumber(DataType.jdbc(JDBCType.OTHER, BigDecimal.class)));
        assertTrue(typeIsNumber(DataType.jdbc(JDBCType.OTHER, BigInteger.class)));

        assertFalse(typeIsNumber(DataType.jdbc(JDBCType.OTHER, String.class)));

    }

    @Test
    public void testNumberByJdbcType() {
        assertFalse(typeIsNumber(null));

        assertTrue(typeIsNumber(DataType.jdbc(JDBCType.NUMERIC, String.class)));
        assertTrue(typeIsNumber(DataType.jdbc(JDBCType.INTEGER, String.class)));
        assertTrue(typeIsNumber(DataType.jdbc(JDBCType.BIGINT, String.class)));
        assertTrue(typeIsNumber(DataType.jdbc(JDBCType.DOUBLE, String.class)));
        assertTrue(typeIsNumber(DataType.jdbc(JDBCType.FLOAT, String.class)));
        assertTrue(typeIsNumber(DataType.jdbc(JDBCType.DECIMAL, String.class)));
        assertTrue(typeIsNumber(DataType.jdbc(JDBCType.BIT, String.class)));

        assertFalse(typeIsNumber(DataType.jdbc(JDBCType.DATE, String.class)));


    }

    @Test
    public void testDateByJdbcType() {
        assertFalse(typeIsDate(null));

        assertTrue(typeIsDate(DataType.jdbc(JDBCType.DATE, String.class)));
        assertTrue(typeIsDate(DataType.jdbc(JDBCType.TIME, String.class)));
        assertTrue(typeIsDate(DataType.jdbc(JDBCType.TIMESTAMP, String.class)));
        assertTrue(typeIsDate(DataType.jdbc(JDBCType.TIME_WITH_TIMEZONE, String.class)));
        assertTrue(typeIsDate(DataType.jdbc(JDBCType.TIMESTAMP_WITH_TIMEZONE, String.class)));

        assertFalse(typeIsNumber(DataType.jdbc(JDBCType.VARCHAR, String.class)));
    }

    @Test
    public void testDateByJavaType() {
        assertFalse(typeIsDate(null));

        assertTrue(typeIsDate(DataType.jdbc(JDBCType.VARCHAR, Date.class)));
        assertTrue(typeIsDate(DataType.jdbc(JDBCType.VARCHAR, java.util.Date.class)));
        assertTrue(typeIsDate(DataType.jdbc(JDBCType.VARCHAR, Timestamp.class)));
        assertTrue(typeIsDate(DataType.jdbc(JDBCType.VARCHAR, LocalDate.class)));
        assertTrue(typeIsDate(DataType.jdbc(JDBCType.VARCHAR, LocalTime.class)));
        assertTrue(typeIsDate(DataType.jdbc(JDBCType.VARCHAR, LocalDateTime.class)));

        assertFalse(typeIsNumber(DataType.jdbc(JDBCType.VARCHAR, String.class)));
    }
}