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
        copyToArray(arr);
        return arr;
    }

    @Override
    @reactor.util.annotation.NonNull
    public <T1> T1[] toArray(T1[] a) {
        int size = size();
        @SuppressWarnings("all")
        T1[] r = a.length >= size ? a :
            (T1[]) Array.newInstance(a.getClass().getComponentType(), size);
        copyToArray(r);
        if (r.length > size) {
            r[size] = null;
        }
        return r;
    }

    private void copyToArray(Object[] target) {
        int index = 0;
        for (List<T> list : nestedList) {
            if (list instanceof RandomAccess) {
                for (int i = 0; i < list.size(); i++) {
                    target[index++] = list.get(i);
                }
            } else {
                // 顺序列表保留迭代器，避免按索引访问退化为平方复杂度。
                for (T value : list) {
                    target[index++] = value;
                }
            }
        }
    }

    @Override
    @reactor.util.annotation.NonNull
    public Iterator<T> iterator() {
        return new FlatListIterator<>(nestedList);
    }

    private static class FlatListIterator<T> implements Iterator<T> {
        private final Iterator<List<T>> outerIterator;
        private List<T> innerList;
        private Iterator<T> innerIterator;
        private int innerIndex;

        public FlatListIterator(List<List<T>> nestedList) {
            this.outerIterator = nestedList.iterator();
            advance();
        }

        private void advance() {
            innerList = outerIterator.hasNext() ? Objects.requireNonNull(outerIterator.next()) : null;
            innerIndex = 0;
            // 顺序列表保留迭代器，避免按索引访问退化为平方复杂度。
            innerIterator = innerList != null && !(innerList instanceof RandomAccess)
                ? innerList.iterator() : null;
        }

        @Override
        public boolean hasNext() {
            while (innerList != null) {
                if (innerIterator == null ? innerIndex < innerList.size() : innerIterator.hasNext()) {
                    return true;
                }
                advance();
            }
            return false;
        }

        @Override
        public T next() {
            if (!hasNext()) {
                throw new NoSuchElementException();
            }
            return innerIterator == null ? innerList.get(innerIndex++) : innerIterator.next();
        }
    }

}
