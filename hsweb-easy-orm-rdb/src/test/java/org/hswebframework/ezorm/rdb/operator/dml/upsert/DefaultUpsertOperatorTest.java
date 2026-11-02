package org.hswebframework.ezorm.rdb.operator.dml.upsert;

import org.junit.Assert;
import org.junit.Test;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class DefaultUpsertOperatorTest {

    @Test
    public void testIgnoreUpdateAndBatchValuesWillNotDuplicateColumns() {
        DefaultUpsertOperator operator = new DefaultUpsertOperator();

        operator.columns("id", "name");
        operator.ignoreUpdate("name");

        List<Map<String, Object>> values = Arrays.asList(
            new LinkedHashMap<String, Object>() {{
                put("id", "1");
                put("name", "n1");
            }},
            new LinkedHashMap<String, Object>() {{
                put("id", "2");
                put("name", "n2");
            }}
        );

        operator.values(values);

        Assert.assertEquals(2, operator.getParameter().getColumns().size());
        Assert.assertArrayEquals(
            new Object[]{"id", "name"},
            operator.getParameter().getColumns().stream().map(UpsertColumn::getColumn).toArray()
        );
        Assert.assertTrue(
            operator.getParameter()
                    .getColumns()
                    .stream()
                    .filter(column -> "name".equals(column.getColumn()))
                    .findFirst()
                    .orElseThrow(AssertionError::new)
                    .isUpdateIgnore()
        );
    }

    @Test
    public void testIgnoreUpdateBeforeBatchValuesShouldTakeEffect() {
        DefaultUpsertOperator operator = new DefaultUpsertOperator();

        operator.ignoreUpdate("id");
        operator.values(Arrays.asList(
            new LinkedHashMap<String, Object>() {{
                put("id", "1");
                put("name", "n1");
            }},
            new LinkedHashMap<String, Object>() {{
                put("id", "2");
                put("name", "n2");
            }}
        ));

        Assert.assertEquals(2, operator.getParameter().getColumns().size());
        Assert.assertTrue(
            operator.getParameter()
                    .getColumns()
                    .stream()
                    .filter(column -> "id".equals(column.getColumn()))
                    .findFirst()
                    .orElseThrow(AssertionError::new)
                    .isUpdateIgnore()
        );
    }

    @Test
    public void testIgnoreUpdateColumnNotPresentInAnyValueShouldNotCreateColumn() {
        DefaultUpsertOperator operator = new DefaultUpsertOperator();

        operator.ignoreUpdate("name");
        operator.values(Arrays.asList(
            new LinkedHashMap<String, Object>() {{
                put("id", "1");
                put("age", 10);
            }},
            new LinkedHashMap<String, Object>() {{
                put("id", "2");
                put("age", 20);
            }}
        ));

        Assert.assertArrayEquals(
            new Object[]{"id", "age"},
            operator.getParameter().getColumns().stream().map(UpsertColumn::getColumn).toArray()
        );
        Assert.assertFalse(
            operator.getParameter()
                    .getColumns()
                    .stream()
                    .anyMatch(column -> "name".equals(column.getColumn()))
        );
    }
}
