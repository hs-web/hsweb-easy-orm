package org.hswebframework.ezorm.core;

import java.sql.SQLException;
import java.util.Collection;

@Deprecated

public interface Insert<T>extends TriggerSkipSupport<Insert<T>> {
    Insert<T> value(T data);

    Insert<T> values(Collection<T> data);

    int exec() throws SQLException;
}
