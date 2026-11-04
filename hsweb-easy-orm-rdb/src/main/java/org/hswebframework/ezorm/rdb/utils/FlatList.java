package org.hswebframework.ezorm.rdb.utils;

import java.lang.reflect.Array;
import java.util.*;
import java.util.function.Consumer;

/**
 * 多个子列表的有序扁平视图，保留各子列表的遍历语义。
 * 子列表仍由调用方持有；整体快照和并发修改由调用方管理。
 */
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
        forEach(new Consumer<T>() {
            private int index;

            @Override
            public void accept(T value) {
                target[index++] = value;
            }
        });
    }

    @Override
    public void forEach(Consumer<? super T> action) {
        Objects.requireNonNull(action);
        // 委托子列表自身的批量遍历，兼顾无迭代器实现与快照语义，不按具体列表类型分支。
        for (List<T> list : nestedList) {
            list.forEach(action);
        }
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
            advance();
        }

        private void advance() {
            // RandomAccess 只描述索引访问成本，不能替代子列表自身的迭代快照。
            innerIterator = outerIterator.hasNext() ? outerIterator.next().iterator() : null;
        }

        @Override
        public boolean hasNext() {
            while (innerIterator != null) {
                if (innerIterator.hasNext()) {
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
            return innerIterator.next();
        }
    }

}
