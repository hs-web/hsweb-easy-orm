package org.hsweb.ezorm.core;

import org.hsweb.ezorm.core.param.QueryParam;

import java.sql.SQLException;
import java.util.List;

public interface Query<T> extends Conditional<Query<T>>{
    <Q extends Query<T>> Q setParam(QueryParam param);

    <Q extends Query<T>> Q select(String... fields);

    <Q extends Query<T>> Q selectExcludes(String... fields);

    <Q extends Query<T>> Q orderByAsc(String column);

    <Q extends Query<T>> Q orderByDesc(String column);

    List<T> list() throws SQLException;

    List<T> list(int pageIndex, int pageSize) throws SQLException;

    T single() throws SQLException;

    int total() throws SQLException;

}
