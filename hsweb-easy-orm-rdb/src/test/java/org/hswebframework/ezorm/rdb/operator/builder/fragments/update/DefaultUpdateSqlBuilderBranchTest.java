package org.hswebframework.ezorm.rdb.operator.builder.fragments.update;

import org.hswebframework.ezorm.core.dsl.Query;
import org.hswebframework.ezorm.rdb.executor.EmptySqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.MetadataHelper;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateColumn;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperatorParameter;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.Collections;

public class DefaultUpdateSqlBuilderBranchTest {
    private DefaultUpdateSqlBuilder builder;

    @Before
    public void init() {
        RDBSchemaMetadata schema = MetadataHelper.createMockSchema();
        builder = DefaultUpdateSqlBuilder.of(schema.getTable("test").orElseThrow(NullPointerException::new));
    }

    @Test
    public void testEmptyColumnsReturnsEmptyRequest() {
        UpdateOperatorParameter parameter = new UpdateOperatorParameter();
        parameter.getWhere().addAll(Query.of().where("id", "1234").getParam().getTerms());
        Assert.assertSame(EmptySqlRequest.INSTANCE, builder.build(parameter));
    }

    @Test(expected = UnsupportedOperationException.class)
    public void testNoWhereFailsFast() {
        UpdateOperatorParameter parameter = new UpdateOperatorParameter();
        UpdateColumn column = new UpdateColumn();
        column.setColumn("name");
        column.setValue("admin");
        parameter.getColumns().add(column);
        builder.build(parameter);
    }

    @Test(expected = UnsupportedOperationException.class)
    public void testNoUpdatableColumnsFailsFast() {
        UpdateOperatorParameter parameter = new UpdateOperatorParameter();
        UpdateColumn column = new UpdateColumn();
        column.setColumn("name");
        column.setValue(null);
        parameter.getColumns().add(column);
        parameter.getWhere().addAll(Query.of().where("id", "1234").getParam().getTerms());
        builder.build(parameter);
    }

    @Test
    public void testNativeValueAndDuplicateBranches() {
        UpdateOperatorParameter parameter = new UpdateOperatorParameter();

        UpdateColumn first = new UpdateColumn();
        first.setColumn("name");
        first.setValue("admin");
        parameter.getColumns().add(first);

        UpdateColumn duplicate = new UpdateColumn();
        duplicate.setColumn("name");
        duplicate.setValue("ignored");
        parameter.getColumns().add(duplicate);

        parameter.getWhere().addAll(Query.of().where("id", "1234").getParam().getTerms());

        SqlRequest request = builder.build(parameter);
        Assert.assertEquals("update \"PUBLIC\".test set \"NAME\" = ? where \"ID\" = ?", request.getSql());
        Assert.assertArrayEquals(new Object[]{"admin", "1234"}, request.getParameters());
    }
}
