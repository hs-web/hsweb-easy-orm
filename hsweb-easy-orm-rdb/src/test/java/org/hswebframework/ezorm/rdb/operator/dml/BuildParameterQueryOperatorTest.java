package org.hswebframework.ezorm.rdb.operator.dml;

import org.hswebframework.ezorm.rdb.operator.dml.query.BuildParameterQueryOperator;
import org.hswebframework.ezorm.rdb.operator.dml.query.Joins;
import org.hswebframework.ezorm.rdb.operator.dml.query.QueryOperatorParameter;
import org.junit.Assert;
import org.junit.Test;

import static org.hswebframework.ezorm.rdb.operator.dml.query.Selects.count;

public class BuildParameterQueryOperatorTest {

    @Test
    public void test() {
        BuildParameterQueryOperator query = new BuildParameterQueryOperator();

        query.select(count("id").as("total"))
                .from("u_user")
                .join(Joins.left("detail").as("info"))
                .where(dsl -> dsl.like("name", "1234"))
                .paging(10, 0);

        QueryOperatorParameter parameter = query.getParameter();

        Assert.assertEquals(parameter.getFrom(), "u_user");
        Assert.assertFalse(parameter.getSelect().isEmpty());
        Assert.assertFalse(parameter.getJoins().isEmpty());
        Assert.assertFalse(parameter.getWhere().isEmpty());
        Assert.assertEquals(parameter.getPageIndex(),Integer.valueOf(10));
        Assert.assertEquals(parameter.getPageSize(),Integer.valueOf(0));
    }

}