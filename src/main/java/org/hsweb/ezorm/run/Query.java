package org.hsweb.ezorm.run;

import org.hsweb.ezorm.param.QueryParam;
import org.hsweb.ezorm.param.Term;

import java.sql.SQLException;
import java.util.List;

public interface Query<T> extends Conditional<Query<T>>, TriggerSkipSupport<Query<T>> {
    Query<T> setParam(QueryParam param);

    Query<T> select(String... fields);

    Query<T> selectExcludes(String... fields);

    Query<T> orderByAsc(String field);

    Query<T> orderByDesc(String field);

    Query<T> noPaging();

    Query<T> forUpdate();

    List<T> list() throws SQLException;

    List<T> list(int pageIndex, int pageSize) throws SQLException;

    T single() throws SQLException;

    int total() throws SQLException;
}
