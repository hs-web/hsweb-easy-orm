package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import org.hswebframework.ezorm.rdb.metadata.*;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.metadata.key.ForeignKeyBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.query.SelectColumnFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.function.SimpleFunctionFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.query.QueryOperatorParameter;
import org.hswebframework.ezorm.rdb.operator.dml.Join;
import org.hswebframework.ezorm.rdb.operator.dml.query.SelectColumn;
import org.hswebframework.ezorm.rdb.operator.dml.query.NativeSelectColumn;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.sql.JDBCType;
import java.util.Arrays;
import java.util.Collections;

import static org.hswebframework.ezorm.rdb.operator.dml.query.SelectColumn.*;

public class SelectColumnFragmentBuilderTest {

    SelectColumnFragmentBuilder builder;

    RDBTableMetadata table;

    @Before
    public void init() {
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.H2);
        RDBSchemaMetadata schema = new RDBSchemaMetadata("DEFAULT");

        database.setCurrentSchema(schema);
        database.addSchema(schema);

        RDBTableMetadata test =schema.newTable("test");
        table = test;
        RDBTableMetadata detail = schema.newTable("detail");
        RDBTableMetadata detail2 = schema.newTable("detail2");

        schema.addTable(test);
        schema.addTable(detail);
        schema.addTable(detail2);

        {
            RDBColumnMetadata id = new RDBColumnMetadata();
            id.setName("id");
            id.setType(JdbcDataType.of(JDBCType.VARCHAR,String.class));
            id.setLength(32);

            RDBColumnMetadata name = new RDBColumnMetadata();
            name.setName("name");
            name.setType(JdbcDataType.of(JDBCType.VARCHAR,String.class));
            name.setLength(64);

            test.addColumn(id);
            test.addColumn(name);
        }
        {

            RDBColumnMetadata detailInfo = new RDBColumnMetadata();
            detailInfo.setName("comment");
            detailInfo.setType(JdbcDataType.of(JDBCType.VARCHAR,String.class));
            detailInfo.setLength(64);

            detail2.addColumn(detailInfo);
        }
        {

            RDBColumnMetadata detailInfo = new RDBColumnMetadata();
            detailInfo.setName("comment");
            detailInfo.setType(JdbcDataType.of(JDBCType.VARCHAR,String.class));
            detailInfo.setLength(64);

            detail.addColumn(detailInfo);
        }
        //逻辑主键
        test.addForeignKey(ForeignKeyBuilder.builder()
                .target("detail")
                .autoJoin(true)
                .build().addColumn("id","comment"));

        builder = SelectColumnFragmentBuilder.of(test);
    }

    @Test
    public void testJoin() {
        SelectColumn column = new SelectColumn();
        column.setColumn("id");
        column.setAlias("_id");


        Join join=new Join();
        join.setTarget("detail");
        join.setAlias("info");
        SelectColumn name = new SelectColumn();
        name.setColumn("detail.comment");

        QueryOperatorParameter parameter = new QueryOperatorParameter();
        parameter.setSelect(Arrays.asList(column,name));
        parameter.setJoins(Arrays.asList(join));

        SqlFragments fragments = builder.createFragments(parameter);
        Assert.assertNotNull(fragments);
        System.out.println(fragments.toRequest().getSql());

    }

    @Test
    public void testFunction() {
        SelectColumn column = new SelectColumn();
//        column.setColumn("id");
        column.setAlias("total");
        column.setFunction("count");
        column.setOpts(Collections.singletonMap("arg","1"));

        QueryOperatorParameter parameter = new QueryOperatorParameter();
        parameter.setSelect(Arrays.asList(column));

        SqlFragments fragments = builder.createFragments(parameter);
        Assert.assertNotNull(fragments);
        System.out.println(fragments.toRequest().getSql());
        Assert.assertEquals(fragments.toRequest().getSql(),"count( 1 ) as \"total\"");

    }

    @Test
    public void testCreateFragments() {
        SelectColumn column = new SelectColumn();
        column.setColumn("id");
        column.setAlias("_id");

        SelectColumn fakeName = new SelectColumn();
        fakeName.setColumn("fake_name");
        fakeName.setAlias("fakeName");
        QueryOperatorParameter parameter = new QueryOperatorParameter();
        parameter.setSelect(Arrays.asList(column,fakeName));

        SqlFragments fragments = builder.createFragments(parameter);
        Assert.assertNotNull(fragments);
        Assert.assertFalse(fragments.toRequest().getSql().contains("fake_name"));
        System.out.println(fragments.toRequest().getSql());

    }

    @Test
    public void testSimple() {
        SelectColumn column = new SelectColumn();
        column.setColumn("id");
        column.setAlias("_id");

        SelectColumn name = new SelectColumn();
        name.setColumn("name");
        name.setAlias("_name");
        QueryOperatorParameter parameter = new QueryOperatorParameter();
        parameter.setSelect(Arrays.asList(column,name));

        SqlFragments fragments = builder.createFragments(parameter);
        Assert.assertNotNull(fragments);
        System.out.println(fragments.toRequest().getSql());

    }


    @Test
    public void testAll() {
        Join join=new Join();
        join.setTarget("detail2");
        join.setAlias("info");

        QueryOperatorParameter parameter = new QueryOperatorParameter();
        parameter.setSelect(Arrays.asList(of("*"), of("detail.*"),of("info.comment")));
        parameter.getSelectExcludes().add("id");
        parameter.getJoins().add(join);

        SqlFragments fragments = builder.createFragments(parameter);
        System.out.println(fragments.toRequest().getSql());
        Assert.assertNotNull(fragments);
        String sql = fragments.toRequest().getSql();
        Assert.assertFalse(sql.contains("id"));
        Assert.assertTrue(sql.contains("name"));
        Assert.assertTrue(sql.contains("detail.comment"));
        Assert.assertTrue(sql.contains("info.comment"));

    }
    @Test
    public void testDefaultSelectUsesAllColumnsAndAliasExcludes() {
        QueryOperatorParameter parameter = new QueryOperatorParameter();
        parameter.getSelectExcludes().add("name");

        SqlFragments fragments = builder.createFragments(parameter);
        Assert.assertEquals("test.\"ID\" as \"id\"", fragments.toRequest().getSql());
    }

    @Test
    public void testNativeSelectColumnPreservesParametersAndAlias() {
        NativeSelectColumn nativeColumn = new NativeSelectColumn("coalesce(?, test.\"NAME\")", new Object[]{"fallback"});
        nativeColumn.setAlias("displayName");

        QueryOperatorParameter parameter = new QueryOperatorParameter();
        parameter.setSelect(Collections.singletonList(nativeColumn));

        SqlRequest request = builder.createFragments(parameter).toRequest();
        Assert.assertEquals("coalesce(?, test.\"NAME\") as \"DISPLAYNAME\"", request.getSql());
        Assert.assertArrayEquals(new Object[]{"fallback"}, request.getParameters());
    }

    @Test
    public void testFunctionBranchesForDistinctUnknownAndEmptyFunction() {
        table.addFeature(new SimpleFunctionFragmentBuilder("sum", "合计"));

        SelectColumn sum = new SelectColumn();
        sum.setColumn("id");
        sum.setAlias("totalId");
        sum.setFunction("sum");
        sum.setOpts(Collections.singletonMap("distinct", true));

        QueryOperatorParameter parameter = new QueryOperatorParameter();
        parameter.setSelect(Collections.singletonList(sum));
        Assert.assertEquals("sum( distinct test.\"ID\" ) as \"totalId\"",
                            builder.createFragments(parameter).toRequest().getSql());

        SelectColumn unknownFunction = new SelectColumn();
        unknownFunction.setColumn("id");
        unknownFunction.setFunction("missing_function");
        parameter.setSelect(Collections.singletonList(unknownFunction));
        Assert.assertTrue(builder.createFragments(parameter).isEmpty());

        SelectColumn emptyFunction = new SelectColumn();
        emptyFunction.setFunction("sum");
        parameter.setSelect(Collections.singletonList(emptyFunction));
        try {
            builder.createFragments(parameter);
            Assert.fail("empty function fragment should fail fast");
        } catch (UnsupportedOperationException e) {
            Assert.assertTrue(e.getMessage().contains("unsupported function"));
        }
    }

    @Test
    public void testJoinWildcardUnknownAliasAndForeignKeyAutoJoinBranches() {
        Join join = new Join();
        join.setTarget("detail2");
        join.setAlias("info");

        QueryOperatorParameter parameter = new QueryOperatorParameter();
        parameter.getJoins().add(join);
        parameter.setSelect(Arrays.asList(of("unknown.*"), of("test.*"), of("info.*"), of("detail.comment")));
        parameter.getSelectExcludes().add("id");

        SqlRequest request = builder.createFragments(parameter).toRequest();
        String sql = request.getSql();
        Assert.assertFalse(sql.contains("unknown"));
        Assert.assertFalse(sql.contains("ID"));
        Assert.assertTrue(sql.contains("test.\"NAME\" as \"name\""));
        Assert.assertTrue(sql.contains("info.\"COMMENT\" as \"info.comment\""));
        Assert.assertTrue(sql.contains("detail.\"COMMENT\" as \"detail.comment\""));
        Assert.assertFalse(parameter.getJoins().isEmpty());
    }

}