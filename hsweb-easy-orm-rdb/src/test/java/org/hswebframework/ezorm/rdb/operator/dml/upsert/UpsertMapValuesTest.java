package org.hswebframework.ezorm.rdb.operator.dml.upsert;

import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

public class UpsertMapValuesTest {

    @Test
    public void shouldKeepSparseColumnOrderAndPublishedHooks() {
        List<Object[]> rows = new ArrayList<>();
        List<String> columns = new ArrayList<>();
        DefaultUpsertOperator operator = new DefaultUpsertOperator() {
            @Override
            public UpsertOperator columns(String... names) {
                columns.addAll(Arrays.asList(names));
                return super.columns(names);
            }

            @Override
            public UpsertOperator values(Object... values) {
                rows.add(values);
                return super.values(values);
            }
        };
        Map<String, Object> first = new LinkedHashMap<>();
        first.put("id", 1L);
        first.put("value", 12.75D);
        first.put("absent", null);
        Map<String, Object> second = new LinkedHashMap<>();
        second.put("enabled", true);
        second.put("id", 2L);
        second.put("text", "sample");
        operator.values(Arrays.asList(Collections.unmodifiableMap(first), second, Collections.emptyMap()));
        assertEquals(Arrays.asList("id", "value", "absent", "enabled", "text"), columns);
        assertEquals(3, rows.size());
        assertArrayEquals(new Object[]{1L, 12.75D, null, null, null}, rows.get(0));
        assertArrayEquals(new Object[]{2L, null, null, true, "sample"}, rows.get(1));
        assertArrayEquals(new Object[5], rows.get(2));
        assertNotSame(rows.get(0), rows.get(1));
        first.put("id", 99L);
        assertEquals(1L, rows.get(0)[0]);
    }

    @Test
    public void shouldKeepEmptyBatchAndEmptyRowBehavior() {
        DefaultUpsertOperator operator = new DefaultUpsertOperator();
        assertSame(operator, operator.values((List<Map<String, Object>>) null));
        assertSame(operator, operator.values(Collections.<Map<String, Object>>emptyList()));
        assertTrue(operator.getParameter().getValues().isEmpty());
        operator.values(Collections.singletonList(Collections.emptyMap()));
        assertEquals(1, operator.getParameter().getValues().size());
        assertTrue(operator.getParameter().getValues().get(0).isEmpty());
    }
}
