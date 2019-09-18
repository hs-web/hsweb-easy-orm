package org.hswebframework.ezorm.rdb.mapping;


public interface SyncUpdate<E> extends DSLUpdate<E, SyncUpdate<E>> {

    int execute();
}
