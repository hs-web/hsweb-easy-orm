package org.hswebframework.ezorm.rdb.utils;

import lombok.Getter;
import lombok.Setter;
import org.junit.Test;

import java.beans.PropertyDescriptor;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

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

    @Test
    public void testConvertListAndGetPropertyField(){
        assertTrue(PropertiesUtils.convertList(null).isEmpty());
        assertEquals(Arrays.asList("a", "b"), PropertiesUtils.convertList(new String[]{"a", "b"}));
        List<Object> list = PropertiesUtils.convertList(Collections.singletonList("x"));
        assertEquals(Collections.singletonList("x"), list);
        assertEquals(Collections.singletonList("y"), PropertiesUtils.convertList("y"));

        assertTrue(PropertiesUtils.getPropertyField(TestEntity.class, "name").isPresent());
        assertTrue(PropertiesUtils.getPropertyField(TestEntity.class, "id").isPresent());
        assertFalse(PropertiesUtils.getPropertyField(TestEntity.class, "missing").isPresent());
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
