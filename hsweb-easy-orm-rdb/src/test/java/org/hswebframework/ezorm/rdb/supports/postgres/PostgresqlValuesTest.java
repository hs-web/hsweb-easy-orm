package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.R2dbcParameterBinder;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.supports.opengauss.OpengaussDialect;
import org.junit.Test;

import java.math.BigDecimal;
import java.sql.JDBCType;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.*;

public class PostgresqlValuesTest {

    static RDBColumnMetadata column(JDBCType type, Class<?> javaType) {
        RDBColumnMetadata column = new RDBColumnMetadata();
        column.setType(JdbcDataType.of(type, javaType));
        return column;
    }

    @Test
    public void preservesTypesNullsPrecisionOrderAndParameterOwnership() {
        List<RDBColumnMetadata> columns = List.of(column(JDBCType.VARCHAR, String.class),
            column(JDBCType.BIGINT, Long.class), column(JDBCType.NUMERIC, BigDecimal.class),
            column(JDBCType.BOOLEAN, Boolean.class), column(JDBCType.INTEGER, Integer.class),
            column(JDBCType.SMALLINT, Short.class), column(JDBCType.REAL, Float.class), column(JDBCType.DOUBLE, Double.class));
        BigDecimal decimal = new BigDecimal("12345678901234567890123456.123456789012");
        Object[] first = {"a,'?\\", Long.MAX_VALUE, decimal, true, 4, (short) 5, 1.25f, 2.5d};
        Object[] second = {null, null, NullValue.of(BigDecimal.class, null), null, null, null, null, null};
        SqlRequest request = Dialect.POSTGRES.buildValues(columns, Arrays.asList(first, second)).toRequest();
        assertEquals(8, request.getParameters().length);
        assertTrue(request.getSql().startsWith("select * from unnest("));
        for (int i = 0; i < columns.size(); i++) {
            PostgresqlArrayParameter parameter = (PostgresqlArrayParameter) request.getParameters()[i];
            assertEquals(first[i], ((Object[]) parameter.getValue())[0]);
            assertNull(((Object[]) parameter.getValue())[1]);
        }
        first[0] = "changed";
        assertEquals("a,'?\\", ((Object[]) ((PostgresqlArrayParameter) request.getParameters()[0]).getValue())[0]);
    }

    @Test
    public void keepsParameterCountIndependentOfBatchSizeAndHandlesAllNulls() {
        List<Object[]> rows = new ArrayList<>();
        for (int i = 0; i < 4096; i++) rows.add(new Object[]{null, (long) i});
        var columns = List.of(column(JDBCType.VARCHAR, String.class), column(JDBCType.BIGINT, Long.class));
        SqlRequest request = Dialect.POSTGRES.buildValues(columns, rows).toRequest();
        assertEquals(2, request.getParameters().length);
        assertEquals(4096, ((Object[]) ((PostgresqlArrayParameter) request.getParameters()[0]).getValue()).length);
        assertEquals(request.getSql(), Dialect.POSTGRES.buildValues(columns, rows.subList(0, 1)).toRequest().getSql());
        assertTrue(Dialect.POSTGRES.buildValues(columns, List.of()).isEmpty());
    }

    @Test
    public void fallsBackForExpressionsAndCustomEncodedParametersWithoutReencoding() {
        var columns = List.of(column(JDBCType.VARCHAR, String.class), column(JDBCType.INTEGER, Integer.class));
        SqlRequest request = Dialect.POSTGRES.buildValues(columns,
            Arrays.asList(new Object[]{"first", 1}, new Object[]{NativeSql.of("upper(?)", "second"), null})).toRequest();
        assertEquals("values (?,?),(upper(?),?)", request.getSql());
        assertEquals("second", request.getParameters()[2]);
        assertEquals(Integer.class, ((NullValue) request.getParameters()[3]).getType());
        R2dbcParameterBinder binder = (statement, identifier) -> statement.bind(identifier, "encoded");
        request = Dialect.POSTGRES.buildValues(List.of(columns.get(0)), List.<Object[]>of(new Object[]{binder})).toRequest();
        assertEquals("values (?)", request.getSql());
        assertSame(binder, request.getParameters()[0]);
    }

    @Test
    public void defaultDialectAndUnsupportedPostgresqlTypesUseValues() {
        for (Dialect dialect : List.of(Dialect.MYSQL, Dialect.H2, Dialect.POSTGRES)) {
            SqlRequest request = dialect.buildValues(List.of(column(JDBCType.TIMESTAMP, java.util.Date.class)),
                Arrays.asList(new Object[]{new java.util.Date(0)}, new Object[]{null})).toRequest();
            assertEquals("values (?),(?)", request.getSql());
            assertEquals(2, request.getParameters().length);
        }
    }

    @Test
    public void opengaussKeepsStandardValuesForScalarBatches() {
        SqlRequest request = OpengaussDialect.global.buildValues(
            List.of(column(JDBCType.VARCHAR, String.class), column(JDBCType.BIGINT, Long.class)),
            Arrays.asList(new Object[]{"first", 1L}, new Object[]{"second", null})).toRequest();
        assertEquals("values (?,?),(?,?)", request.getSql());
        assertArrayEquals(new Object[]{"first", 1L, "second"},
            Arrays.copyOf(request.getParameters(), 3));
        assertEquals(Long.class, ((NullValue) request.getParameters()[3]).getType());
    }

    @Test
    public void rejectsUnequalRowsInsteadOfUnnestNullPadding() {
        assertThrows(IllegalArgumentException.class, () -> Dialect.POSTGRES.buildValues(
            List.of(column(JDBCType.VARCHAR, String.class)), List.<Object[]>of(new Object[0])));
        assertThrows(IllegalArgumentException.class, () -> Dialect.MYSQL.buildValues(
            List.of(column(JDBCType.VARCHAR, String.class)), List.<Object[]>of(new Object[]{1, 2})));
    }

    @Test
    public void numericAndBooleanArrayCodecsPreserveValues() {
        assertArrayEquals(new BigDecimal[]{new BigDecimal("1.250"), null},
            (Object[]) PostgresqlArrayType.NUMERIC_ARRAY.decode("{1.250,NULL}"));
        assertArrayEquals(new Boolean[]{true, false, null},
            (Object[]) PostgresqlArrayType.BOOLEAN_ARRAY.decode("{true,false,NULL}"));
        assertArrayEquals(new Float[]{1.25f, null}, (Object[]) PostgresqlArrayType.REAL_ARRAY.encode(Arrays.asList(1.25, null)));
        assertArrayEquals(new Double[]{1.25d, null}, (Object[]) PostgresqlArrayType.DOUBLE_ARRAY.encode(Arrays.asList(1.25, null)));
    }
}
