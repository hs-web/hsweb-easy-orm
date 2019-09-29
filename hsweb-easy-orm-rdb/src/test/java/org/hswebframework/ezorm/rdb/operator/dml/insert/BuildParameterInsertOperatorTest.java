package org.hswebframework.ezorm.rdb.operator.dml.insert;

import org.junit.Assert;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;

import static org.junit.Assert.*;

public class BuildParameterInsertOperatorTest {

    @Test
    public void test() {
        BuildParameterInsertOperator operator = new BuildParameterInsertOperator();


        operator.columns("id", "name")
                .values(1, "2");

        InsertOperatorParameter parameter = operator.getParameter();

        assertArrayEquals(parameter.getColumns().stream().map(InsertColumn::getColumn).toArray(), new Object[]{"id", "name"});
        assertArrayEquals(parameter.getValues().stream().flatMap(Collection::stream).toArray(), new Object[]{1, "2"});


    }

    @Test
    public void testBatchInsert(){
        BuildParameterInsertOperator operator = new BuildParameterInsertOperator();

        operator.values(Arrays.asList(new HashMap<String, Object>(){
            {
                put("name","123");
                put("age",1);
                put("comment","test");

            }
        },new HashMap<String, Object>(){
            {
                put("name","123");
                put("age",1);

            }
        }));


        Assert.assertEquals(operator.getParameter().getColumns().size(),3);

        Assert.assertEquals(operator.getParameter().getValues().size(),2);

        Assert.assertEquals(operator.getParameter().getValues().get(0).size(),3);
        Assert.assertEquals(operator.getParameter().getValues().get(1).size(),3);

    }

}