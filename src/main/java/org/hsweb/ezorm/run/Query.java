package org.hsweb.ezorm.run;

import org.hsweb.ezorm.param.QueryParam;
import org.hsweb.ezorm.param.Term;

import java.sql.SQLException;
import java.util.List;

/**
 * Created by zhouhao on 16-6-4.
 */
public interface Query<T> extends TriggerSkipSupport<Query<T>> {
    Query<T> setParam(QueryParam param);

    Query<T> select(String... fields);

    Query<T> selectExcludes(String... fields);

    Query<T> where(String condition, Object value);

    Query<T> and(String condition, Object value);

    Query<T> or(String condition, Object value);

    Term nest();

    Term nest(String condition, Object value);

    Term orNest(String condition, Object value);

    Query<T> orderByAsc(String field);

    Query<T> orderByDesc(String field);

    Query<T> noPaging();

    Query<T> forUpdate();

    List<T> list() throws SQLException;

    List<T> list(int pageIndex, int pageSize) throws SQLException;

    T single() throws SQLException;

    int total() throws SQLException;
}
