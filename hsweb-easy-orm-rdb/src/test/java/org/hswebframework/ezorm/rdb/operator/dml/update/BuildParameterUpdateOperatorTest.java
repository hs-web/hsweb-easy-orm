package org.hswebframework.ezorm.rdb.operator.dml.update;

import org.junit.Test;

import static org.junit.Assert.*;

public class BuildParameterUpdateOperatorTest {

    @Test
    public void test() {
        BuildParameterUpdateOperator update = new BuildParameterUpdateOperator();

        update.set("name", "1234")
                .where(dsl -> dsl.and("id", "1"));

        UpdateOperatorParameter parameter=update.getParameter();

        UpdateColumn column = new UpdateColumn();
        column.setColumn("name");

        assertTrue(parameter.getColumns().contains(column));

        assertEquals(parameter.getWhere().get(0).getValue(),"1");
        assertEquals(parameter.getWhere().get(0).getColumn(),"id");

    }
}