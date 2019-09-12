package org.hswebframework.ezorm.core.mapping;

import org.hswebframework.ezorm.core.Conditional;

import java.util.List;

public interface SyncQuery<T> extends Conditional<SyncQuery<T>> {

    List<T> fetch();



    int count();

    

}
