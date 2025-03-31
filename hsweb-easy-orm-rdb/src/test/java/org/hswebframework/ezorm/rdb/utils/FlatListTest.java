package org.hswebframework.ezorm.rdb.utils;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.*;

public class FlatListTest {


    @Test
    public void test() {
        FlatList<Integer> list = new FlatList<>(
            Arrays.asList(
                Arrays.asList(1, 2, 3),
                Arrays.asList(4, 5, 6),
                Arrays.asList(7, 8, 9)
            )
        );
        list.listIterator(3).forEachRemaining(System.out::println);

        assertEquals(Integer.valueOf(1), list.get(0));
        assertEquals(Integer.valueOf(2), list.get(1));

        assertEquals(4, list.get(3).intValue());
        assertEquals(7, list.get(6).intValue());

        assertEquals(Arrays.asList(3, 4, 5), list.subList(2, 5));

        assertArrayEquals(new Integer[]{1, 2, 3, 4, 5, 6, 7, 8, 9}, list.toArray());

    }
}