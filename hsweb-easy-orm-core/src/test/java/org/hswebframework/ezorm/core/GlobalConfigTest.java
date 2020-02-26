package org.hswebframework.ezorm.core;

import lombok.Getter;
import lombok.Setter;
import org.junit.Assert;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.*;

public class GlobalConfigTest {

    @Test
    public void test() {
        Assert.assertNotNull(GlobalConfig.getObjectConverter());

        Assert.assertNotNull(GlobalConfig.getPropertyOperator());
    }


    @Test
    public void testConvert() {
        Map<String, Object> map = new HashMap<>();
        TestEntity entity = new TestEntity();
        entity.setName("1234");
        GlobalConfig.getObjectConverter().convert(entity, () -> map);
        Assert.assertEquals(map.get("name"), "1234");

        TestEntity newEntity = GlobalConfig.getObjectConverter().convert(map, TestEntity.class);
        Assert.assertEquals(newEntity.getName(), "1234");

    }

    @Test
    public void testMap() {
        Map<String, Object> map = new HashMap<>();
        map.put("test", "1234");
        Assert.assertEquals(GlobalConfig.getPropertyOperator().getProperty(map, "test").orElse(null), "1234");

        GlobalConfig.getPropertyOperator().setProperty(map, "test", "12345");

        Assert.assertEquals(map.get("test"), "12345");
    }

    @Test
    public void testEntity() {
        TestEntity entity = new TestEntity();
        GlobalConfig.getPropertyOperator().setProperty(entity, "name", "12345");

        Assert.assertEquals(entity.getName(), "12345");

        Assert.assertEquals(GlobalConfig.getPropertyOperator().getProperty(entity, "name").orElse(null), "12345");

    }

    @Getter
    @Setter
    public static class TestEntity {
        private String name;
    }
}