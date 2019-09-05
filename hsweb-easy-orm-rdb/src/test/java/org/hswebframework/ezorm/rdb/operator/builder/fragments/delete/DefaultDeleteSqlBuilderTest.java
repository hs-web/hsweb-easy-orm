package org.hswebframework.ezorm.rdb.operator.builder.fragments.delete;

import org.hswebframework.ezorm.core.dsl.Query;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.MetadataHelper;
import org.hswebframework.ezorm.rdb.operator.dml.delete.DeleteOperatorParameter;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class DefaultDeleteSqlBuilderTest {

    DefaultDeleteSqlBuilder builder;

    @Before
    public void init() {
        RDBSchemaMetadata schema = MetadataHelper.createMockSchema();

        builder = DefaultDeleteSqlBuilder.of(schema.getTable("test").orElseThrow(NullPointerException::new));
    }

    @Test
    public void test(){
        DeleteOperatorParameter parameter=new DeleteOperatorParameter();

        {
            parameter.getWhere().addAll(Query.of().where("id", "1234").and().notNull("name").getParam().getTerms());
        }

        SqlRequest sql= builder.build(parameter);
        System.out.println(sql);
        Assert.assertEquals(sql.getSql(),"delete from PUBLIC.test where id = ? and name not null");

    }
}