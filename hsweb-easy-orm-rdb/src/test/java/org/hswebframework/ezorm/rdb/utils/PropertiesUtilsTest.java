package org.hswebframework.ezorm.rdb.utils;

import lombok.Getter;
import lombok.Setter;
import org.junit.Test;

import java.beans.PropertyDescriptor;
import java.util.Arrays;

import static org.junit.Assert.*;

public class PropertiesUtilsTest {


    @Test
    public void test(){
        PropertyDescriptor[] descriptor = PropertiesUtils.getDescriptors(TestEntity.class);

        assertEquals(3,descriptor.length);

        for (PropertyDescriptor propertyDescriptor : descriptor) {
            System.out.println(propertyDescriptor.getName());
        }
    }

    @Getter
    @Setter
    public static class TestEntity extends SuperEntity{

        private String name;

        private String aTest;
    }

    @Getter
    @Setter
    public static class SuperEntity{
        private String id;

    }
}