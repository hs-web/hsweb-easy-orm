package org.hswebframework.ezorm.core;

import org.junit.Test;
import java.util.List;
import java.util.Map;
import static org.junit.Assert.*;

public class SimplePropertyWrapperJsonTest {
    @Test
    public void jsonAndAlreadyBoundValues() {
        Map<String, Object> map = Map.of("name", "test");
        assertSame(map, new SimplePropertyWrapper(map).toMap());
        List<String> list = List.of("test");
        assertSame(list, new SimplePropertyWrapper(list).toBeanList(String.class));
        assertEquals("test", new SimplePropertyWrapper(map).toBean(Item.class).name);
        assertEquals("test", new SimplePropertyWrapper("{\"name\":\"test\"}").toBean(Item.class).name);
        assertEquals(List.of(1, 2), new SimplePropertyWrapper("[1,2]").toBeanList(Integer.class));
        assertNull(new SimplePropertyWrapper("   ").toBean(Item.class));
        assertNull(new SimplePropertyWrapper(null).toBean(Item.class));
    }

    @Test
    public void pojosAndArraysUseProviderConversionWithoutTextIntermediate() {
        Item item = new Item();
        item.name = "中文😀";
        assertSame(item, new SimplePropertyWrapper(item).toBean(Item.class));
        assertEquals(Map.of("name", "中文😀"), new SimplePropertyWrapper(item).toMap());
        List<Item> items = new SimplePropertyWrapper(new Item[]{item}).toBeanList(Item.class);
        assertEquals(1, items.size());
        assertEquals("中文😀", items.get(0).name);
        assertNull(new SimplePropertyWrapper(null).toBeanList(Item.class));
        assertNull(new SimplePropertyWrapper("null").toBean(Item.class));
    }

    public static class Item {
        public String name;
    }
}
