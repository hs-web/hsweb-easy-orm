package org.hswebframework.ezorm.rdb.operator.builder.fragments.insert;

import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.MetadataHelper;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertColumn;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperatorParameter;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.Arrays;
import java.util.concurrent.atomic.AtomicInteger;

public class BatchInsertSqlBuilderTest {

    private BatchInsertSqlBuilder builder;

    private RDBSchemaMetadata schema;

    @Before
    public void init() {
        schema = MetadataHelper.createMockSchema();

        builder = BatchInsertSqlBuilder.of(schema.getTable("test").orElseThrow(NullPointerException::new));
    }


    @Test
    public void testDefaultValue() {
        schema.getTable("test")
                .flatMap(table->table.getColumn("id"))
        .ifPresent(id->id.setDefaultValue((RuntimeDefaultValue) () -> "runtime_id"));

        InsertOperatorParameter insert = new InsertOperatorParameter();
        {
            insert.getColumns().add(InsertColumn.of("id"));
        }
        insert.getValues().add(Arrays.asList(new Object[]{null}));

        SqlRequest request = builder.build(insert);
        System.out.println(request);
        Assert.assertArrayEquals(request.getParameters(), new Object[]{"runtime_id"});
    }

    @Test
    public void testNullValuePrimaryKeyUsesRuntimeDefaultForEachRow() {
        RDBTableMetadata table = schema.getTable("test").orElseThrow(NullPointerException::new);
        RDBColumnMetadata id = table.getColumn("id").orElseThrow(NullPointerException::new);
        AtomicInteger idSequence = new AtomicInteger();
        id.setPrimaryKey(true);
        id.setDefaultValue((RuntimeDefaultValue) () -> "runtime_id_" + idSequence.incrementAndGet());

        InsertOperatorParameter insert = new InsertOperatorParameter();
        insert.getColumns().add(InsertColumn.of("id"));
        insert.getColumns().add(InsertColumn.of("name"));
        insert.getValues().add(Arrays.asList(NullValue.of(id.getType()), "test1"));
        insert.getValues().add(Arrays.asList(NullValue.of(id.getType()), "test2"));

        SqlRequest request = builder.build(insert);
        Assert.assertArrayEquals(
            new Object[]{"runtime_id_1", "test1", "runtime_id_2", "test2"},
            request.getParameters());
    }

    @Test
    public void testCompositePrimaryKeyDeduplicateByOrderedValues() {
        RDBTableMetadata table = schema.getTable("test").orElseThrow(NullPointerException::new);
        table.getColumn("id").orElseThrow(NullPointerException::new).setPrimaryKey(true);
        table.getColumn("name").orElseThrow(NullPointerException::new).setPrimaryKey(true);

        InsertOperatorParameter insert = new InsertOperatorParameter();
        insert.getColumns().add(InsertColumn.of("id"));
        insert.getColumns().add(InsertColumn.of("name"));
        insert.getValues().add(Arrays.asList("a", "b"));
        insert.getValues().add(Arrays.asList("b", "a"));
        insert.getValues().add(Arrays.asList("a", "b"));

        SqlRequest request = builder.build(insert);
        Assert.assertArrayEquals(new Object[]{"a", "b", "b", "a"}, request.getParameters());
    }

    @Test
    public void test() {
        InsertOperatorParameter insert = new InsertOperatorParameter();
        {
            InsertColumn column = new InsertColumn();
            column.setColumn("id");
            insert.getColumns().add(column);
        }
        {
            InsertColumn column = new InsertColumn();
            column.setColumn("name");
            insert.getColumns().add(column);
        }

        insert.getValues().add(Arrays.asList("1", "2"));
        insert.getValues().add(Arrays.asList("3", "4"));

        SqlRequest request = builder.build(insert);
        Assert.assertArrayEquals(request.getParameters(), new Object[]{"1", "2", "3", "4"});
        System.out.println(request);

    }
}
