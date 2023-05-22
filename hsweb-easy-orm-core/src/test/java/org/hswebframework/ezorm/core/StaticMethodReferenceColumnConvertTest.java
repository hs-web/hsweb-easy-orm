package org.hswebframework.ezorm.core;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.MethodReferenceConverter;
import org.junit.Assert;
import org.junit.Test;

import java.io.Serializable;
import java.util.function.BiConsumer;
import java.util.function.Function;

public class StaticMethodReferenceColumnConvertTest {


    @Test
    public void testConvertLambdaColumn() {

        TestClass testClass = new TestClass();

        Assert.assertEquals(MethodReferenceConverter.convertToColumn(TestClass::setEnabled), "enabled");


        Assert.assertEquals(MethodReferenceConverter.convertToColumn(TestClass::getName), "name");

        Assert.assertEquals(MethodReferenceConverter.convertToColumn(TestClass::isEnabled), "enabled");

        Assert.assertEquals(MethodReferenceConverter.convertToColumn(testClass::getName), "name");

        Assert.assertEquals(MethodReferenceConverter.convertToColumn(testClass::isEnabled), "enabled");


    }

    @Test
    public void testSuper() {

        TestClass clazz = new TestClass();
        Object[] refs = new Object[]{
                (StaticMethodReferenceColumn<TestClass>) TestClass::getId,
                (SetterMethodReferenceColumn<TestClass, String>) TestClass::setId,
               // (MethodReferenceColumn<String>) (clazz::getId),
        };
        {
            for (Object ref : refs) {
                MethodReferenceInfo info = MethodReferenceConverter.parse(ref);
                Assert.assertEquals(info.getOwner(), TestClass.class);
            }
        }

    }

    @Getter
    @Setter

    public static class TestClass extends SuperClass {
        private String name;

        private boolean enabled;
    }

    @Getter
    @Setter
    public static class SuperClass implements Serializable {
        private String id;

    }
}