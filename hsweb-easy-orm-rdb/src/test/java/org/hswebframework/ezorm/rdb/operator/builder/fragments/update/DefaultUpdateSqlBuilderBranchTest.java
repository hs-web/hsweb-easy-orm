package org.hswebframework.ezorm.rdb.operator.builder.fragments.update;

import org.hswebframework.ezorm.core.dsl.Query;
import org.hswebframework.ezorm.rdb.executor.EmptySqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.MetadataHelper;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SimpleSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.function.FunctionFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateColumn;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperatorParameter;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.Map;

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

    @Test
    public void mixedFunctionAndOrdinaryColumnsKeepOrderWhenSkippingColumns() {
        for (boolean functionFirst : new boolean[]{false, true}) {
            RDBTableMetadata table = MetadataHelper.createMockSchema().getTable("test").orElseThrow(AssertionError::new);
            table.getColumnNow("id").setUpdatable(false);
            table.getColumnNow("comment").addFeature(new FunctionFragmentBuilder() {
                @Override public String getFunction() { return "probe"; }
                @Override public String getName() { return "probe"; }
                @Override public SqlFragments create(String name, RDBColumnMetadata column, Map<String, Object> options) {
                    return SimpleSqlFragments.of(Collections.singletonList("upper(?)"), Collections.singletonList("function-value"));
                }
            });
            UpdateOperatorParameter parameter = new UpdateOperatorParameter();
            for (String name : functionFirst ? Arrays.asList("id", "comment", "name") : Arrays.asList("name", "id", "comment")) {
                UpdateColumn column = new UpdateColumn();
                column.setColumn(name);
                column.setValue("value");
                if ("comment".equals(name)) {
                    column.setFunction("probe");
                }
                parameter.getColumns().add(column);
            }
            parameter.getWhere().addAll(Query.of().where("id", "one").getParam().getTerms());

            SqlRequest request = DefaultUpdateSqlBuilder.of(table).build(parameter);
            Assert.assertEquals("update \"PUBLIC\".test set " + (functionFirst
                ? "\"COMMENT\" = upper(?) , \"NAME\" = ?" : "\"NAME\" = ? , \"COMMENT\" = upper(?)") + " where \"ID\" = ?", request.getSql());
            Assert.assertArrayEquals(functionFirst ? new Object[]{"function-value", "value", "one"}
                : new Object[]{"value", "function-value", "one"}, request.getParameters());
        }
    }
}
