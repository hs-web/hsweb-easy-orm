package org.hswebframework.ezorm.rdb.operator.dml.insert;

import org.junit.Test;

import java.util.Collection;

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

}