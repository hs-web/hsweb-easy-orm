package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import org.hswebframework.ezorm.rdb.metadata.*;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.query.SelectColumnFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.query.QueryOperatorParameter;
import org.hswebframework.ezorm.rdb.operator.dml.Join;
import org.hswebframework.ezorm.rdb.operator.dml.query.SelectColumn;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.sql.JDBCType;
import java.util.Arrays;
import java.util.Collections;

import static org.hswebframework.ezorm.rdb.operator.dml.query.SelectColumn.*;

public class SelectColumnFragmentBuilderTest {

    SelectColumnFragmentBuilder builder;

    @Before
    public void init() {
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.H2);
        RDBSchemaMetadata schema = new RDBSchemaMetadata("DEFAULT");

        database.setCurrentSchema(schema);
        database.addSchema(schema);

        RDBTableMetadata test =schema.newTable("test");
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
                .targetColumn("comment")
                .sourceColumn("id")
                .autoJoin(true)
                .build());

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
        Assert.assertEquals(fragments.toRequest().getSql(),"count(1) as \"total\"");

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
}