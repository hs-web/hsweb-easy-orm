package org.hswebframework.ezorm.rdb.operator.dml;

import org.junit.Assert;
import org.junit.Test;

import static org.hswebframework.ezorm.rdb.operator.dml.query.Selects.count;

public class DefaultQueryOperatorTest {

    @Test
    public void test() {
        DefaultQueryOperator query = new DefaultQueryOperator();

        query.select(count("id").as("total"))
                .from("u_user")
                .leftJoin("u_detail", join -> join.as("detail").on("detail.id = u_user.id"))
                .where(dsl -> dsl.like("name", "1234"))
                .limit(10, 0);

        ComplexQueryParameter parameter = query.getParameter();

        Assert.assertEquals(parameter.getFrom(), "u_user");
        Assert.assertFalse(parameter.getSelect().isEmpty());
        Assert.assertFalse(parameter.getJoins().isEmpty());
        Assert.assertFalse(parameter.getWhere().isEmpty());
        Assert.assertEquals(parameter.getLimit(),Integer.valueOf(10));
        Assert.assertEquals(parameter.getOffset(),Integer.valueOf(0));
    }

}