package org.hswebframework.ezorm.rdb.operator.builder.fragments.insert;

import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.rdb.executor.BatchSqlRequest;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.MetadataHelper;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SimpleSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.function.FunctionFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertColumn;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperatorParameter;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

public class MultiInsertSqlBuilderBranchTest {
    private MultiInsertSqlBuilder builder;
    private RDBSchemaMetadata schema;

    @Before
    public void init() {
        schema = MetadataHelper.createMockSchema();
        builder = MultiInsertSqlBuilder.of(schema.getTable("test").orElseThrow(NullPointerException::new));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testNoOperableColumnsFailsFast() {
        builder.build(new InsertOperatorParameter());
    }

    @Test
    public void testRuntimeDefaultNativeAndNullBranches() {
        schema.getTable("test").orElseThrow().getColumn("id").ifPresent(column -> column.setDefaultValue((RuntimeDefaultValue) () -> "generated-id"));
        schema.getTable("test").orElseThrow().getColumn("comment").ifPresent(column -> column.setDefaultValue((RuntimeDefaultValue) () -> "fallback-comment"));

        InsertOperatorParameter parameter = new InsertOperatorParameter();
        parameter.getColumns().add(insertColumn("id"));
        parameter.getColumns().add(insertColumn("name"));
        parameter.getColumns().add(insertColumn("comment"));
        parameter.getValues().add(Arrays.asList(null, "JetLinks", org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql.of("upper(?)", "x")));

        SqlRequest request = builder.build(parameter);
        Assert.assertTrue(request.getSql().startsWith("insert into \"PUBLIC\".test"));
        Assert.assertArrayEquals(new Object[]{"generated-id", "JetLinks", "x"}, request.getParameters());
    }

    @Test
    public void skippedColumnsKeepOriginalIndexesAtEveryPosition() {
        String[] names = {"id", "name", "comment"};
        String[] columns = {"\"NAME\" , \"COMMENT\"", "\"ID\" , \"COMMENT\"", "\"ID\" , \"NAME\""};
        Object[][] expected = {{"name-value", "comment-value"}, {"id-value", "comment-value"}, {"id-value", "name-value"}};
        for (int skipped = 0; skipped < names.length; skipped++) {
            RDBTableMetadata table = table();
            table.getColumnNow(names[skipped]).setInsertable(false);
            InsertOperatorParameter parameter = insert(names);
            parameter.getValues().add(Arrays.asList("id-value", "name-value", "comment-value"));
            parameter.getValues().add(Arrays.asList("next-id", "next-name", "next-comment"));

            BatchSqlRequest request = (BatchSqlRequest) MultiInsertSqlBuilder.of(table).build(parameter);
            Assert.assertEquals(sql(columns[skipped], "? , ?"), request.getSql());
            Assert.assertArrayEquals(expected[skipped], request.getParameters());
            List<Object> next = new ArrayList<>(Arrays.asList("next-id", "next-name", "next-comment"));
            next.remove(skipped);
            Assert.assertArrayEquals(next.toArray(), request.getBatch().get(0).getParameters());
        }
    }

    @Test
    public void unknownColumnsKeepOriginalIndexesAtEveryPosition() {
        for (int skipped = 0; skipped <= 3; skipped++) {
            List<String> columns = new ArrayList<>(Arrays.asList("id", "name", "comment"));
            List<Object> values = new ArrayList<>(Arrays.asList("one", "name-value", "comment-value"));
            columns.add(skipped, "unknown");
            values.add(skipped, "ignored");
            InsertOperatorParameter parameter = insert(columns.toArray(new String[0]));
            parameter.getValues().add(values);

            SqlRequest request = MultiInsertSqlBuilder.of(table()).build(parameter);
            Assert.assertEquals(sql("\"ID\" , \"NAME\" , \"COMMENT\"", "? , ? , ?"), request.getSql());
            Assert.assertArrayEquals(new Object[]{"one", "name-value", "comment-value"}, request.getParameters());
        }
    }

    @Test
    public void functionsReplaceValuesAtEveryPositionWithoutDefaultsOrEncoding() {
        String[] names = {"id", "name", "comment"};
        String[] values = {"upper(?) , ? , ?", "? , upper(?) , ?", "? , ? , upper(?)"};
        Object[][] expected = {{"function-value", "name-value", "comment-value"}, {"id-value", "function-value", "comment-value"}, {"id-value", "name-value", "function-value"}};
        for (int position = 0; position < names.length; position++) {
            RDBTableMetadata table = table();
            RDBColumnMetadata column = table.getColumnNow(names[position]);
            addFunction(column);
            column.setDefaultValue((RuntimeDefaultValue) () -> { throw new AssertionError("Function must replace default"); });
            column.setValueCodec(new ValueCodec<Object, Object>() {
                @Override public Object encode(Object value) { throw new AssertionError("Function must replace value encoding"); }
                @Override public Object encodeNull() { throw new AssertionError("Function must replace null encoding"); }
                @Override public Object decode(Object value) { return value; }
            });
            InsertOperatorParameter parameter = insert(names);
            for (InsertColumn input : parameter.getColumns()) {
                if (names[position].equals(input.getColumn())) {
                    input.setFunction("probe");
                }
            }
            List<Object> row = new ArrayList<>(Arrays.asList("id-value", "name-value", "comment-value"));
            row.set(position, null);
            parameter.getValues().add(row);
            parameter.getValues().add(Collections.emptyList());

            BatchSqlRequest request = (BatchSqlRequest) MultiInsertSqlBuilder.of(table).build(parameter);
            Assert.assertEquals(sql("\"ID\" , \"NAME\" , \"COMMENT\"", values[position]), request.getSql());
            Assert.assertArrayEquals(expected[position], request.getParameters());
            Object[] missing = request.getBatch().get(0).getParameters();
            Assert.assertEquals(3, missing.length);
            Assert.assertEquals("function-value", missing[position]);
            for (int index = 0; index < missing.length; index++) {
                if (index != position) {
                    Assert.assertTrue(missing[index] instanceof NullValue);
                }
            }
        }
    }

    @Test
    public void functionAfterSkippedColumnUsesItsOriginalIndex() {
        RDBTableMetadata table = table();
        table.getColumnNow("id").setInsertable(false);
        addFunction(table.getColumnNow("name"));
        InsertOperatorParameter parameter = insert("id", "unknown", "name", "comment");
        for (InsertColumn column : parameter.getColumns()) {
            if ("name".equals(column.getColumn())) {
                column.setFunction("probe");
            }
        }
        parameter.getValues().add(Arrays.asList("ignored-id", "ignored-unknown", "ignored-name", "comment-value"));

        SqlRequest request = MultiInsertSqlBuilder.of(table).build(parameter);
        Assert.assertEquals(sql("\"NAME\" , \"COMMENT\"", "upper(?) , ?"), request.getSql());
        Assert.assertArrayEquals(new Object[]{"function-value", "comment-value"}, request.getParameters());
    }

    @Test
    public void shortRowsAndExplicitNullUseDefaultsWithoutShiftingColumns() {
        RDBTableMetadata table = table();
        table.getColumnNow("id").setInsertable(false);
        AtomicInteger generated = new AtomicInteger();
        table.getColumnNow("name").setDefaultValue((RuntimeDefaultValue) () -> "default-" + generated.incrementAndGet());
        InsertOperatorParameter parameter = insert("id", "name", "comment");
        parameter.getValues().add(Collections.emptyList());
        parameter.getValues().add(Arrays.asList("ignored", "provided"));
        parameter.getValues().add(Arrays.asList("ignored", NullValue.of(table.getColumnNow("name").getType()), NativeSql.of("upper(?)", "comment")));

        BatchSqlRequest request = (BatchSqlRequest) MultiInsertSqlBuilder.of(table).build(parameter);
        Assert.assertEquals(sql("\"NAME\" , \"COMMENT\"", "? , ?"), request.getSql());
        Assert.assertEquals("default-1", request.getParameters()[0]);
        Assert.assertTrue(request.getParameters()[1] instanceof NullValue);
        Assert.assertEquals(String.class, ((NullValue) request.getParameters()[1]).getType());
        Assert.assertEquals("provided", request.getBatch().get(0).getParameters()[0]);
        Assert.assertTrue(request.getBatch().get(0).getParameters()[1] instanceof NullValue);
        Assert.assertEquals(sql("\"NAME\" , \"COMMENT\"", "? , upper(?)"), request.getBatch().get(1).getSql());
        Assert.assertArrayEquals(new Object[]{"default-2", "comment"}, request.getBatch().get(1).getParameters());
        Assert.assertEquals(2, generated.get());
    }

    @Test
    public void ordinaryAndEncodedExpressionsKeepParametersIsolatedAcrossRows() {
        RDBTableMetadata table = table();
        AtomicInteger encoded = new AtomicInteger();
        table.getColumnNow("name").setValueCodec(new ValueCodec<Object, Object>() {
            @Override public Object encode(Object value) {
                encoded.incrementAndGet();
                return NativeSql.of("coalesce(?, ?)", value, "fallback");
            }
            @Override public Object decode(Object value) { return value; }
        });
        InsertOperatorParameter parameter = insert("id", "unknown", "name", "comment");
        parameter.getValues().add(Arrays.asList("one", "ignored", "encoded", "comment"));
        parameter.getValues().add(Arrays.asList(NativeSql.of("lower(?)", "TWO"), "ignored", NativeSql.of("upper(?)", "explicit"), "last"));

        BatchSqlRequest request = (BatchSqlRequest) MultiInsertSqlBuilder.of(table).build(parameter);
        Assert.assertEquals(sql("\"ID\" , \"NAME\" , \"COMMENT\"", "? , coalesce(?, ?) , ?"), request.getSql());
        Assert.assertArrayEquals(new Object[]{"one", "encoded", "fallback", "comment"}, request.getParameters());
        Assert.assertEquals(sql("\"ID\" , \"NAME\" , \"COMMENT\"", "lower(?) , upper(?) , ?"), request.getBatch().get(0).getSql());
        Assert.assertArrayEquals(new Object[]{"TWO", "explicit", "last"}, request.getBatch().get(0).getParameters());
        Assert.assertEquals(1, encoded.get());
    }

    @Test(expected = IllegalArgumentException.class)
    public void allUnknownOrNonInsertableColumnsFail() {
        RDBTableMetadata table = table();
        table.getColumnNow("id").setInsertable(false);
        InsertOperatorParameter parameter = insert("unknown", "id");
        parameter.getValues().add(Arrays.asList("ignored", "ignored-id"));
        MultiInsertSqlBuilder.of(table).build(parameter);
    }

    private RDBTableMetadata table() {
        return MetadataHelper.createMockSchema().getTable("test").orElseThrow(AssertionError::new);
    }

    private InsertOperatorParameter insert(String... names) {
        InsertOperatorParameter parameter = new InsertOperatorParameter();
        for (String name : names) {
            parameter.getColumns().add(insertColumn(name));
        }
        return parameter;
    }

    private String sql(String columns, String values) {
        return "insert into \"PUBLIC\".test ( " + columns + " ) values ( " + values + " )";
    }

    private void addFunction(RDBColumnMetadata column) {
        column.addFeature(new FunctionFragmentBuilder() {
            @Override public String getFunction() { return "probe"; }
            @Override public String getName() { return "probe"; }
            @Override public SqlFragments create(String name, RDBColumnMetadata metadata, Map<String, Object> options) {
                return SimpleSqlFragments.of(Collections.singletonList("upper(?)"), Collections.singletonList("function-value"));
            }
        });
    }

    private InsertColumn insertColumn(String column) {
        InsertColumn insertColumn = new InsertColumn();
        insertColumn.setColumn(column);
        return insertColumn;
    }
}
