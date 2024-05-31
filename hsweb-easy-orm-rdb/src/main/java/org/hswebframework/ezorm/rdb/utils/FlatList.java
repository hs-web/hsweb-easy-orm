package org.hswebframework.ezorm.rdb.utils;

import java.lang.reflect.Array;
import java.util.*;

public class FlatList<T> extends AbstractList<T> implements Iterable<T> {
    private final List<List<T>> nestedList;
    private int size = -1; // Cache size

    public FlatList(List<List<T>> nestedList) {
        this.nestedList = nestedList;
    }

    @Override
    public T get(int index) {
        int currentIndex = 0;
        for (List<T> list : nestedList) {
            if (index < currentIndex + list.size()) {
                return list.get(index - currentIndex);
            }
            currentIndex += list.size();
        }
        throw new IndexOutOfBoundsException("Index: " + index + ", Size: " + size());
    }

    @Override
    public boolean add(T t) {
        size = -1;
        return nestedList.get(nestedList.size() - 1).add(t);
    }

    @Override
    public boolean addAll(Collection<? extends T> c) {
        size = -1;
        return nestedList.add(c instanceof List ? ((List) c) : new ArrayList<>(c));
    }


    @Override
    public T set(int index, T element) {
        int currentIndex = 0;
        for (List<T> list : nestedList) {
            if (index < currentIndex + list.size()) {
                return list.set(index - currentIndex, element);
            }
            currentIndex += list.size();
        }
        throw new IndexOutOfBoundsException("Index: " + index + ", Size: " + size());
    }

    @Override
    public int size() {
        if (size == -1) {
            size = 0;
            for (List<T> list : nestedList) {
                size += list.size();
            }
        }
        return size;
    }

    @Override
    @reactor.util.annotation.NonNull
    public Object[] toArray() {
        Object[] arr = new Object[size()];
        int index = 0;
        for (List<T> ts : nestedList) {
            for (T t : ts) {
                arr[index++] = t;
            }
        }
        return arr;
    }

    @Override
    @reactor.util.annotation.NonNull
    public <T1> T1[] toArray(T1[] a) {
        int size = size();
        @SuppressWarnings("all")
        T1[] r = a.length >= size ? a :
            (T1[]) Array.newInstance(a.getClass().getComponentType(), size);
        int index = 0;
        for (List<T> ts : nestedList) {
            for (T t : ts) {
                r[index++] = (T1) t;
            }
        }
        return r;
    }

    @Override
    @reactor.util.annotation.NonNull
    public Iterator<T> iterator() {
        return new FlatListIterator<>(nestedList);
    }

    private static class FlatListIterator<T> implements Iterator<T> {
        private final Iterator<List<T>> outerIterator;
        private Iterator<T> innerIterator;

        public FlatListIterator(List<List<T>> nestedList) {
            this.outerIterator = nestedList.iterator();
            this.innerIterator = outerIterator.hasNext() ? outerIterator.next().iterator() : null;
        }

        @Override
        public boolean hasNext() {
            while (innerIterator != null && !innerIterator.hasNext()) {
                if (outerIterator.hasNext()) {
                    innerIterator = outerIterator.next().iterator();
                } else {
                    innerIterator = null;
                }
            }
            return innerIterator != null;
        }

        @Override
        public T next() {
            if (!hasNext()) {
                throw new NoSuchElementException();
            }
            return innerIterator.next();
        }
    }

}