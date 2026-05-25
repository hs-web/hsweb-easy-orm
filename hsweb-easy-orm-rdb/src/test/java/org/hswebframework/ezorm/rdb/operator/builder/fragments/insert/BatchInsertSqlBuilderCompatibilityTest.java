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
}
