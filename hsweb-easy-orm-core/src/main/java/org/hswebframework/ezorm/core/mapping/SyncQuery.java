package org.hswebframework.ezorm.core.mapping;

import org.hswebframework.ezorm.core.Conditional;

import java.util.List;
import java.util.Optional;

public interface SyncQuery<T> extends Conditional<SyncQuery<T>> {


    SyncQuery<T> paging(int pageIndex, int pageSize);

    List<T> fetch();

    Optional<T> fetchOne();

    int count();

}
