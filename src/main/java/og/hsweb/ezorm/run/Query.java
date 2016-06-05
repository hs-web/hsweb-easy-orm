package og.hsweb.ezorm.run;

import og.hsweb.ezorm.param.QueryParam;
import og.hsweb.ezorm.param.Term;

import java.sql.SQLException;
import java.util.List;

/**
 * Created by zhouhao on 16-6-4.
 */
public interface Query<T> {
    Query setParam(QueryParam param);

    Query select(String... fields);

    Query selectExcludes(String... fields);

    Query where(String condition, Object value);

    Query and(String condition, Object value);

    Query or(String condition, Object value);

    Term nest();

    Term nest(String condition, Object value);

    Term orNest(String condition, Object value);

    Query orderByAsc(String field);

    Query orderByDesc(String field);

    Query noPaging();

    List<T> list() throws SQLException;

    List<T> list(int pageIndex, int pageSize) throws SQLException;

    T single() throws SQLException;

    int total() throws SQLException;
}
