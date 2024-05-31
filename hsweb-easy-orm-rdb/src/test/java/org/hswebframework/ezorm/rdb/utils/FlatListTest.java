package org.hswebframework.ezorm.rdb.utils;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.*;

public class FlatListTest {


    @Test
    public void test() {
        List<Integer> list = new FlatList<>(
            Arrays.asList(
                Arrays.asList(1, 2, 3),
                Arrays.asList(4, 5, 6),
                Arrays.asList(7, 8, 9)
            )
        );
        list.forEach(System.out::println);

        assertEquals(Integer.valueOf(1), list.get(0));
        assertEquals(Integer.valueOf(2), list.get(1));

        assertArrayEquals(list.toArray(), new Integer[]{1, 2, 3, 4, 5, 6, 7, 8, 9});

    }
}