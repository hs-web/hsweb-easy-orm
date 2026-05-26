package org.hswebframework.ezorm.rdb.operator.builder.fragments.insert;

import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.MetadataHelper;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertColumn;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperatorParameter;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.Arrays;

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

    private InsertColumn insertColumn(String column) {
        InsertColumn insertColumn = new InsertColumn();
        insertColumn.setColumn(column);
        return insertColumn;
    }
}
