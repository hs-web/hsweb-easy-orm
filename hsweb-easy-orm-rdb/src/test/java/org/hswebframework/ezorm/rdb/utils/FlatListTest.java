package org.hswebframework.ezorm.rdb.utils;

import org.hswebframework.ezorm.core.utils.StringUtils;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import static org.junit.Assert.*;

public class FlatListTest {

    @Test
    public void shouldPreserveSnapshotIteratorsForRandomAccessViews() {
        for (boolean wrapped : new boolean[]{false, true}) {
            List<String> source = new CopyOnWriteArrayList<>(Arrays.asList("before", "second"));
            List<String> input = wrapped ? Collections.unmodifiableList(source) : source;
            Iterator<String> iterator = new FlatList<>(Collections.singletonList(input)).iterator();
            source.set(0, "after");
            source.remove(1);
            List<String> result = new ArrayList<>();
            iterator.forEachRemaining(result::add);
            assertEquals(Arrays.asList("before", "second"), result);

            source.clear();
            Iterator<String> empty = new FlatList<>(Collections.singletonList(input)).iterator();
            source.add("later");
            assertFalse(empty.hasNext());
        }
    }

    @Test
    public void shouldPreserveChildSnapshotWhenForEachMutatesItsSource() {
        for (boolean wrapped : new boolean[]{false, true}) {
            List<String> source = new CopyOnWriteArrayList<>(Arrays.asList("first", "second"));
            List<String> input = wrapped ? Collections.unmodifiableList(source) : source;
            List<String> result = new ArrayList<>();
            new FlatList<>(Collections.singletonList(input)).forEach(value -> {
                result.add(value);
                source.clear();
            });
            assertEquals(Arrays.asList("first", "second"), result);
        }
    }

    @Test
    public void shouldPreserveChildSnapshotWhileJoiningSqlFragments() {
        for (boolean wrapped : new boolean[]{false, true}) {
            List<Object> source = new CopyOnWriteArrayList<>();
            Object first = new Object() {
                @Override
                public String toString() {
                    source.set(1, "changed");
                    source.remove(2);
                    return "first";
                }
            };
            source.addAll(Arrays.asList(first, "second", "third"));
            List<Object> input = wrapped ? Collections.unmodifiableList(source) : source;
            assertEquals("first second third", StringUtils.join(" ", new FlatList<>(Collections.singletonList(input))));
        }
    }

    @Test
    public void shouldPreserveChildTraversalWhenConvertingToArray() {
        List<Integer> source = new ArrayList<Integer>(Arrays.asList(1, 2, 3)) {
            @Override
            public Integer get(int index) {
                throw new AssertionError("RandomAccess must not bypass child traversal");
            }
        };
        FlatList<Integer> list = new FlatList<>(Collections.singletonList(source));
        assertArrayEquals(new Object[]{1, 2, 3}, list.toArray());
        assertArrayEquals(new Integer[]{1, 2, 3}, list.toArray(new Integer[0]));
    }

    @Test
    public void shouldConvertMixedListsAndNullsToIndependentArrays() {
        List<String> source = new ArrayList<>(Arrays.asList("a", null));
        FlatList<String> list = new FlatList<>(Arrays.asList(
            Collections.emptyList(), source, new LinkedList<>(Arrays.asList("b", "c")),
            new FlatList<>(Arrays.asList(Collections.emptyList(), Collections.singletonList("d")))));
        Object[] expected = {"a", null, "b", "c", "d"};
        Object[] actual = list.toArray();
        assertArrayEquals(expected, actual);
        assertArrayEquals(expected, list.toArray(new String[0]));
        String[] exact = new String[list.size()];
        assertSame(exact, list.toArray(exact));
        assertArrayEquals(expected, exact);
        String[] larger = {"old", "old", "old", "old", "old", "old", "untouched"};
        assertSame(larger, list.toArray(larger));
        assertArrayEquals(new String[]{"a", null, "b", "c", "d", null, "untouched"}, larger);
        actual[0] = "changed";
        assertEquals("a", source.get(0));
        assertThrows(ArrayStoreException.class, () -> list.toArray(new Integer[0]));
        String[] empty = {"old", "untouched"};
        assertSame(empty, new FlatList<String>(Collections.emptyList()).toArray(empty));
        assertArrayEquals(new String[]{null, "untouched"}, empty);
    }

    @Test
    public void shouldNotIndexSequentialListsWhenConvertingToArray() {
        LinkedList<Integer> sequential = new LinkedList<Integer>(Arrays.asList(1, 2, 3)) {
            @Override
            public Integer get(int index) {
                throw new AssertionError("sequential list must use its iterator");
            }
        };
        FlatList<Integer> list = new FlatList<>(Collections.singletonList(sequential));
        assertArrayEquals(new Integer[]{1, 2, 3}, list.toArray());
        assertArrayEquals(new Integer[]{1, 2, 3}, list.toArray(new Integer[0]));
    }

    @Test
    public void shouldTraverseNestedAndSequentialListsInOrder() {
        FlatList<String> inner = new FlatList<>(Arrays.asList(
            Collections.emptyList(), Collections.singletonList(""), Arrays.asList(null, "a")));
        FlatList<String> list = new FlatList<>(Arrays.asList(
            Collections.emptyList(), inner, new LinkedList<>(Arrays.asList("b", "c")),
            Collections.emptyList()));
        List<String> values = new ArrayList<>();
        list.forEach(values::add);
        assertEquals(Arrays.asList("", null, "a", "b", "c"), values);
        assertEquals(" null a b c", StringUtils.join(" ", list));
        assertEquals(" null a b c", StringUtils.join(" ", list));
        java.util.Iterator<String> iterator = list.iterator();
        for (String value : values) {
            assertTrue(iterator.hasNext());
            assertTrue(iterator.hasNext());
            assertEquals(value, iterator.next());
        }
        assertFalse(iterator.hasNext());
        assertThrows(java.util.NoSuchElementException.class, iterator::next);
        assertThrows(NullPointerException.class,
                     () -> new FlatList<>(Collections.emptyList()).forEach(null));
    }


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
