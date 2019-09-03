package org.hswebframework.ezorm.rdb.operator.builder.fragments.insert;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.meta.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.MetadataHelper;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertColumn;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperatorParameter;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.Arrays;

public class BatchInsertSqlBuilderTest {

    private BatchInsertSqlBuilder builder;

    @Before
    public void init() {
        RDBSchemaMetadata schema = MetadataHelper.createMockSchema();

        builder = BatchInsertSqlBuilder.of(schema.getTable("test").orElseThrow(NullPointerException::new));
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