package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import org.hswebframework.ezorm.rdb.dialect.Dialect;
import org.hswebframework.ezorm.rdb.meta.DefaultRDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.meta.DefaultRDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.meta.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.meta.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.dml.ComplexQueryParameter;
import org.hswebframework.ezorm.rdb.operator.dml.Join;
import org.hswebframework.ezorm.rdb.operator.dml.SelectColumn;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.sql.JDBCType;
import java.util.Arrays;
import java.util.Collections;

public class SelectColumnFragmentBuilderTest {

    SelectColumnFragmentBuilder builder;

    @Before
    public void init() {
        DefaultRDBDatabaseMetadata database = new DefaultRDBDatabaseMetadata(Dialect.H2);
        DefaultRDBSchemaMetadata schema = new DefaultRDBSchemaMetadata();
        schema.setName("DEFAULT");

        database.setCurrentSchema(schema);
        database.addSchema(schema);

        RDBTableMetadata table = new RDBTableMetadata();
        table.setName("test");
        RDBTableMetadata detail = new RDBTableMetadata();
        detail.setName("detail");

        schema.addTable(table);
        schema.addTable(detail);

        RDBColumnMetadata id = new RDBColumnMetadata();
        id.setName("id");
        id.setJdbcType(JDBCType.VARCHAR);
        id.setLength(32);

        RDBColumnMetadata name = new RDBColumnMetadata();
        name.setName("name");
        name.setJdbcType(JDBCType.VARCHAR);
        name.setLength(64);

        table.addColumn(id);
        table.addColumn(name);

        RDBColumnMetadata detailInfo = new RDBColumnMetadata();
        detailInfo.setName("comment");
        detailInfo.setJdbcType(JDBCType.VARCHAR);
        detailInfo.setLength(64);

        detail.addColumn(detailInfo);

        builder = SelectColumnFragmentBuilder.of(table);
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

        ComplexQueryParameter parameter = new ComplexQueryParameter();
        parameter.setSelect(Arrays.asList(column,name));
        parameter.setJoins(Arrays.asList(join));

        SqlFragments fragments = builder.createFragments(parameter);
        Assert.assertNotNull(fragments);
        System.out.println(fragments.toRequest().getSql());

    }

    @Test
    public void testFunction() {
        SelectColumn column = new SelectColumn();
        column.setColumn("id");
        column.setAlias("total");
        column.setFunction("count");
        column.setOpts(Collections.singletonMap("arg","1"));

        ComplexQueryParameter parameter = new ComplexQueryParameter();
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
        ComplexQueryParameter parameter = new ComplexQueryParameter();
        parameter.setSelect(Arrays.asList(column,name));

        SqlFragments fragments = builder.createFragments(parameter);
        Assert.assertNotNull(fragments);
        System.out.println(fragments.toRequest().getSql());

    }
}