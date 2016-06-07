package org.hsweb.ezorm.run;

import java.sql.SQLException;
import java.util.Collection;

/**
 * Created by zhouhao on 16-6-4.
 */
public interface Insert<T>extends TriggerSkipSupport<Insert<T>> {
    Insert<T> value(T data);

    Insert<T> values(Collection<T> data);

    int exec() throws SQLException;
}
