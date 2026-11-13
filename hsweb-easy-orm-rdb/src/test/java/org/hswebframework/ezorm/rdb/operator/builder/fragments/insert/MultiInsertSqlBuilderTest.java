package org.hswebframework.ezorm.rdb.operator.builder.fragments.insert;

import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.rdb.executor.BatchSqlRequest;
import org.hswebframework.ezorm.rdb.executor.EmptySqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.MetadataHelper;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertColumn;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperatorParameter;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.Arrays;

public class MultiInsertSqlBuilderTest {

    private MultiInsertSqlBuilder builder;

    @Before
    public void init() {
        RDBSchemaMetadata schema = MetadataHelper.createMockSchema();

        builder = MultiInsertSqlBuilder.of(schema.getTable("test").orElseThrow(NullPointerException::new));
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
        insert.getValues().add(Arrays.asList("3", NativeSql.of("1+1","4")));

        SqlRequest request = builder.build(insert);
        System.out.println(request);
        Assert.assertTrue(request instanceof BatchSqlRequest);

        BatchSqlRequest batch = ((BatchSqlRequest) request);
        Assert.assertArrayEquals(batch.getParameters(),new Object[]{"1","2"});
        Assert.assertArrayEquals(batch.getBatch().get(0).getParameters(),new Object[]{"3","4"});

    }

    @Test
    public void encodedExpressionsAndNullKeepBatchParameterOrder() {
        RDBTableMetadata table = MetadataHelper.createMockSchema().getTable("test").orElseThrow(AssertionError::new);
        table.getColumnNow("name").setValueCodec(new ValueCodec<Object, Object>() {
            @Override public Object encode(Object value) { return NativeSql.of("upper(?)", value); }
            @Override public Object encodeNull() { return NativeSql.of("null"); }
            @Override public Object decode(Object value) { return value; }
        });
        InsertOperatorParameter insert = new InsertOperatorParameter();
        for (String name : Arrays.asList("id", "name")) {
            InsertColumn column = new InsertColumn();
            column.setColumn(name);
            insert.getColumns().add(column);
        }
        insert.getValues().add(Arrays.asList("one", "encoded"));
        insert.getValues().add(Arrays.asList("two", null));
        insert.getValues().add(Arrays.asList("three", NativeSql.of("lower(?)", "explicit")));
        BatchSqlRequest result = (BatchSqlRequest) MultiInsertSqlBuilder.of(table).build(insert);
        Assert.assertTrue(result.getSql(), result.getSql().contains("upper(?)"));
        Assert.assertArrayEquals(new Object[] {"one", "encoded"}, result.getParameters());
        Assert.assertTrue(result.getBatch().get(0).getSql().contains("null"));
        Assert.assertArrayEquals(new Object[] {"two"}, result.getBatch().get(0).getParameters());
        Assert.assertTrue(result.getBatch().get(1).getSql().contains("lower(?)"));
        Assert.assertArrayEquals(new Object[] {"three", "explicit"}, result.getBatch().get(1).getParameters());
    }
}
