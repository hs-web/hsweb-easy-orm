package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import org.hswebframework.ezorm.core.param.SqlTerm;
import org.hswebframework.ezorm.rdb.meta.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.MetadataHelper;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.query.JoinFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.query.QueryOperatorParameter;
import org.hswebframework.ezorm.rdb.operator.dml.Join;
import org.junit.Before;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;

public class JoinFragmentBuilderTest {

    JoinFragmentBuilder builder;

    @Before
    public void init() {
        RDBSchemaMetadata schema = MetadataHelper.createMockSchema();

        builder = JoinFragmentBuilder.of(schema.getTableOrView("test").orElseThrow(NullPointerException::new));

    }


    @Test
    public void test(){
        QueryOperatorParameter parameter=new QueryOperatorParameter();
        Join join=new Join();
        join.setTarget("detail");
        join.setAlias("_detail");
        join.setTerms(Collections.singletonList(new SqlTerm("_detail.id=test.id")));
        parameter.setJoins(Arrays.asList(join));

        SqlFragments fragments= builder.createFragments(parameter);

        System.out.println(fragments.toRequest().getSql());
    }

    @Test
    public void testTerm(){
        QueryOperatorParameter parameter=new QueryOperatorParameter();
        Join join=new Join();
        join.setTarget("detail");
        join.setAlias("_detail");
        join.setTerms(Collections.singletonList(new SqlTerm("_detail.id=test.id").and("detail.comment","1234")));
        parameter.setJoins(Collections.singletonList(join));

        SqlFragments fragments= builder.createFragments(parameter);

        System.out.println(fragments.toRequest().getSql());
    }

}