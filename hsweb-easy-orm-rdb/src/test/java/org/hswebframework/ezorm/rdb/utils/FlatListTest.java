package org.hswebframework.ezorm.rdb.utils;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.*;

public class FlatListTest {


    @Test
    public void test() {
        java.util.List<java.util.List<Integer>> nested = new java.util.ArrayList<>();
        nested.add(new java.util.ArrayList<>(Arrays.asList(1, 2, 3)));
        nested.add(new java.util.ArrayList<>(Arrays.asList(4, 5, 6)));
        nested.add(new java.util.ArrayList<>(Arrays.asList(7, 8, 9)));
        FlatList<Integer> list = new FlatList<>(nested);
        list.listIterator(3).forEachRemaining(System.out::println);

        assertEquals(Integer.valueOf(1), list.get(0));
        assertEquals(Integer.valueOf(2), list.get(1));

        assertEquals(4, list.get(3).intValue());
        assertEquals(7, list.get(6).intValue());

        assertEquals(Arrays.asList(3, 4, 5), list.subList(2, 5));

        assertArrayEquals(new Integer[]{1, 2, 3, 4, 5, 6, 7, 8, 9}, list.toArray());

        list.set(0, 10);
        assertEquals(Integer.valueOf(10), list.get(0));
        list.add(11);
        list.addAll(Arrays.asList(12, 13));
        assertEquals(12, list.size());
        assertArrayEquals(new Integer[]{10, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13}, list.toArray(new Integer[0]));
        Integer[] larger = new Integer[20];
        assertSame(larger, list.toArray(larger));
        assertEquals(Integer.valueOf(13), larger[11]);

        try {
            list.get(99);
            fail("out of range index should fail");
        } catch (IndexOutOfBoundsException e) {
            assertTrue(e.getMessage().contains("Index: 99"));
        }

        java.util.Iterator<Integer> empty = new FlatList<Integer>(java.util.Collections.emptyList()).iterator();
        assertFalse(empty.hasNext());
        try {
            empty.next();
            fail("empty iterator should fail");
        } catch (java.util.NoSuchElementException ignore) {
        }

    }
}