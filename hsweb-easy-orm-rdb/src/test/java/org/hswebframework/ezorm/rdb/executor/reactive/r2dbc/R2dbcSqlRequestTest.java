package org.hswebframework.ezorm.rdb.executor.reactive.r2dbc;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.junit.Test;

import java.util.Collections;

import static org.junit.Assert.*;

public class R2dbcSqlRequestTest {

    @Test
    public void shouldPreserveLexicalRulesAndNativeNamedParameters() {
        Object[] values = {1, 2, 3};
        SqlRequest request = SqlRequests.prepare(
            "select '?', \"?\", 'it''s?', data ? 'key', data ?| array['a'], data ?& array['b'], "
                + "$2, $1, $2, ? /* ? */ , ? -- ?\n, ?", values);
        SqlRequest converted = R2dbcSqlRequest.of(0, "@arg", request);
        assertEquals("select '?', \"?\", 'it''s?', data ? 'key', data ?| array['a'], data ?& array['b'], "
            + "$2, $1, $2, @arg0 /* ? */ , @arg1 -- ?\n, @arg2", converted.getSql());
        assertSame(values, converted.getParameters());
    }

    @Test
    public void shouldPreserveMultiDigitNegativeAndOverflowedNumbering() {
        for (int first : new int[]{0, 1, -1, -6400, Integer.MIN_VALUE, Integer.MAX_VALUE - 1}) {
            int count = 6400;
            Object[] values = new Object[count];
            String sql = String.join(",", Collections.nCopies(count, "?"));
            String[] expected = new String[count];
            for (int i = 0; i < count; i++) {
                expected[i] = "@p" + (first + i);
            }
            SqlRequest converted = R2dbcSqlRequest.of(first, "@p", SqlRequests.prepare(sql, values));
            assertEquals(String.join(",", expected), converted.getSql());
            assertSame(values, converted.getParameters());
        }
    }

    @Test
    public void shouldRetainUnparameterizedRequestAndParameterizedNativeSql() {
        SqlRequest empty = SqlRequests.prepare("select 1");
        assertSame(empty, R2dbcSqlRequest.of(1, "$", empty));
        Object[] values = {"value-1", "value-2"};
        SqlRequest nativeSql = R2dbcSqlRequest.of(1, "$", SqlRequests.prepare("select $2,$1,$2", values));
        assertEquals("select $2,$1,$2", nativeSql.getSql());
        assertSame(values, nativeSql.getParameters());
    }
}
