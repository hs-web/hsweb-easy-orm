package org.hsweb.ezorm.core;

import org.hsweb.ezorm.core.param.UpdateParam;

import java.sql.SQLException;

public interface Update<T> extends Conditional<Update<T>>, TriggerSkipSupport<Update<T>> {
    Update<T> set(T data);

    Update<T> set(String property, Object value);

    Update<T> includes(String... fields);

    Update<T> excludes(String... fields);

    Update<T> setParam(UpdateParam param);

    int exec() throws SQLException;
}
