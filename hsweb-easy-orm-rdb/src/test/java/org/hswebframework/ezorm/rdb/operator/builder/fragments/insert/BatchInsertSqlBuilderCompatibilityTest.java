package org.hswebframework.ezorm.rdb.operator.builder.fragments.insert;

import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.MetadataHelper;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.AppendableSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.BatchSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SimpleSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.function.FunctionFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertColumn;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperatorParameter;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

public class BatchInsertSqlBuilderCompatibilityTest {

    private BatchInsertSqlBuilder builder;
    private RDBSchemaMetadata schema;

    @Before
    public void init() {
        schema = MetadataHelper.createMockSchema();
        builder = BatchInsertSqlBuilder.of(schema.getTable("test").orElseThrow(NullPointerException::new));
    }

    @Test
    public void testSingleRowNullPrimaryKeyWithRuntimeDefault() {
        RDBTableMetadata table = schema.getTable("test").orElseThrow(NullPointerException::new);
        RDBColumnMetadata id = table.getColumn("id").orElseThrow(NullPointerException::new);
        AtomicInteger seq = new AtomicInteger();
        id.setDefaultValue((RuntimeDefaultValue) () -> "pk-" + seq.incrementAndGet());

        InsertOperatorParameter insert = new InsertOperatorParameter();
        insert.getColumns().add(InsertColumn.of("id"));
        insert.getColumns().add(InsertColumn.of("name"));
        insert.getValues().add(Arrays.asList(NullValue.of(id.getType()), "n1"));

        SqlRequest request = builder.build(insert);
        Assert.assertArrayEquals(new Object[]{"pk-1", "n1"}, request.getParameters());
    }

    @Test
    public void testCompositeKeyWithNullValueDoesNotDeduplicateByPartialKey() {
        RDBTableMetadata table = schema.getTable("test").orElseThrow(NullPointerException::new);
        table.getColumn("id").orElseThrow(NullPointerException::new).setPrimaryKey(true);
        table.getColumn("name").orElseThrow(NullPointerException::new).setPrimaryKey(true);

        InsertOperatorParameter insert = new InsertOperatorParameter();
        insert.getColumns().add(InsertColumn.of("id"));
        insert.getColumns().add(InsertColumn.of("name"));
        insert.getValues().add(Arrays.asList("a", NullValue.of(table.getColumn("name").orElseThrow().getType())));
        insert.getValues().add(Arrays.asList("a", NullValue.of(table.getColumn("name").orElseThrow().getType())));

        SqlRequest request = builder.build(insert);
        Assert.assertEquals(4, request.getParameters().length);
    }

    @Test
    public void shouldPreserveExpressionReferencesAndBuildHooksBetweenOrdinaryParameters() {
        RDBTableMetadata table = schema.getTable("test").orElseThrow();
        List<Object[]> encodedParameters = new ArrayList<>();
        RDBColumnMetadata encoded = new RDBColumnMetadata() {
            @Override
            public Object encode(Object value) {
                Object[] parameters = {value};
                encodedParameters.add(parameters);
                return NativeSql.of("lower(?)", parameters);
            }
        };
        encoded.setName("encoded");
        encoded.setType(table.getColumn("id").orElseThrow().getType());
        table.addColumn(encoded);
        for (String name : Arrays.asList("function", "tail")) {
            RDBColumnMetadata column = new RDBColumnMetadata();
            column.setName(name);
            column.setType(encoded.getType());
            table.addColumn(column);
        }
        List<String> functionSql = new ArrayList<>(Arrays.asList("upper(?)"));
        List<Object> functionParameters = new ArrayList<>(Arrays.asList("function-original"));
        table.getColumn("function").orElseThrow().addFeature(new FunctionFragmentBuilder() {
            @Override
            public String getFunction() {
                return "custom";
            }

            @Override
            public String getName() {
                return "custom";
            }

            @Override
            public SqlFragments create(String name, RDBColumnMetadata column, Map<String, Object> opts) {
                return SimpleSqlFragments.of(functionSql, functionParameters);
            }
        });
        Object[] firstNative = {"native-1a", "native-1b"};
        Object[] secondNative = {"native-2a", "native-2b"};
        Object[] prefixParameters = {"prefix-original"};
        List<String> prefixSql = new ArrayList<>(Arrays.asList("with hook as (select ?)"));
        AtomicInteger rowHooks = new AtomicInteger();
        builder = new BatchInsertSqlBuilder(table) {
            @Override
            protected AppendableSqlFragments beforeBuild(InsertOperatorParameter parameter, AppendableSqlFragments sql) {
                Assert.assertTrue(sql instanceof BatchSqlFragments);
                sql.addSql(prefixSql).addParameter(prefixParameters);
                return super.beforeBuild(parameter, sql);
            }

            @Override
            protected void afterValues(Set<InsertColumn> columns, List<Object> values, AppendableSqlFragments sql) {
                int row = rowHooks.incrementAndGet();
                List<Object> parameters = sql.getParameters();
                Assert.assertEquals("tail-" + row, parameters.get(parameters.size() - 1));
                parameters.set(parameters.size() - 1, "tail-hook-" + row);
            }

            @Override
            protected AppendableSqlFragments afterBuild(Set<InsertColumn> columns, InsertOperatorParameter parameter,
                                                         AppendableSqlFragments sql) {
                prefixSql.set(0, "with extension as (select ?)");
                prefixParameters[0] = "prefix-final";
                firstNative[0] = "native-1a-final";
                secondNative[1] = "native-2b-final";
                functionSql.set(0, "lower(?)");
                functionParameters.set(0, "function-final");
                encodedParameters.get(0)[0] = "encoded-1-final";
                encodedParameters.get(1)[0] = "encoded-2-final";
                return sql.addSql("returning ?").addParameter("suffix");
            }
        };
        InsertOperatorParameter insert = new InsertOperatorParameter();
        for (String name : Arrays.asList("id", "name", "comment", "function", "encoded", "tail")) {
            InsertColumn column = InsertColumn.of(name);
            if (name.equals("function")) {
                column.setFunction("custom");
            }
            insert.getColumns().add(column);
        }
        insert.getValues().add(Arrays.asList("id-1", NativeSql.of("coalesce(?,?)", firstNative),
                                            "comment-1", "ignored", "encoded-1", "tail-1"));
        insert.getValues().add(Arrays.asList("id-2", NativeSql.of("coalesce(?,?)", secondNative),
                                            "comment-2", "ignored", "encoded-2", "tail-2"));
        SqlRequest request = builder.build(insert);
        Assert.assertEquals(2, rowHooks.get());
        Assert.assertTrue(request.getSql().startsWith("with extension as (select ?)"));
        Assert.assertFalse(request.getSql().contains("upper(?)"));
        Assert.assertArrayEquals(new Object[]{"prefix-final",
            "id-1", "native-1a-final", "native-1b", "comment-1", "function-final", "encoded-1-final", "tail-hook-1",
            "id-2", "native-2a", "native-2b-final", "comment-2", "function-final", "encoded-2-final", "tail-hook-2",
            "suffix"}, request.getParameters());
    }

    @Test
    public void shouldKeepRowsIndependentWhenColumnsOrDuplicateKeysAreSkipped() {
        RDBTableMetadata table = schema.getTable("test").orElseThrow();
        table.getColumn("id").orElseThrow().setPrimaryKey(true);
        table.getColumn("name").orElseThrow().setInsertable(false);
        InsertOperatorParameter insert = new InsertOperatorParameter();
        for (String name : Arrays.asList("id", "name", "comment")) {
            insert.getColumns().add(InsertColumn.of(name));
        }
        insert.getValues().add(Arrays.asList("id-1", "ignored-1", "comment-1"));
        insert.getValues().add(Arrays.asList("id-2", "ignored-2", "comment-2"));
        insert.getValues().add(Arrays.asList("id-1", "ignored-3", "comment-3"));
        Assert.assertArrayEquals(new Object[]{"id-1", "comment-1", "id-2", "comment-2"},
                                 builder.build(insert).getParameters());
    }
}
