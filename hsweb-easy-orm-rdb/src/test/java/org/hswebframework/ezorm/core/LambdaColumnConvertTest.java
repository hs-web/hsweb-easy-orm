package org.hswebframework.ezorm.core;

import lombok.Getter;
import lombok.Setter;
import org.junit.Assert;
import org.junit.Test;

import static org.junit.Assert.*;

public class LambdaColumnConvertTest {


    @Test
    public void testConvertLambdaColumn(){
        Assert.assertEquals(LambdaColumnConvert.convertToColumn(TestClass::getName),"name");

        Assert.assertEquals(LambdaColumnConvert.convertToColumn((LambdaColumn<Object>) o -> null),"enabled");

    }

    @Getter
    @Setter

    public static class TestClass{
        private String name;

        private boolean enabled;
    }

}