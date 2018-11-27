package org.hswebframework.ezorm.core;

import lombok.Getter;
import lombok.Setter;
import org.junit.Assert;
import org.junit.Test;

import java.io.Serializable;

public class StaticMethodReferenceColumnConvertTest {


    @Test
    public void testConvertLambdaColumn() {

        TestClass testClass = new TestClass();


        Assert.assertEquals(MethodReferenceConvert.convertToColumn(TestClass::getName), "name");

        Assert.assertEquals(MethodReferenceConvert.convertToColumn(TestClass::isEnabled), "enabled");

        Assert.assertEquals(MethodReferenceConvert.convertToColumn(testClass::getName), "name");

        Assert.assertEquals(MethodReferenceConvert.convertToColumn(testClass::isEnabled), "enabled");

    }

    @Getter
    @Setter

    public static class TestClass implements Serializable {
        private String name;

        private boolean enabled;
    }

}