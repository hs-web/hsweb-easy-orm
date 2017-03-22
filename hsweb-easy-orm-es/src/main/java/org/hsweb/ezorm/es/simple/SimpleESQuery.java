package org.hsweb.ezorm.es.simple;

import org.elasticsearch.client.RestClient;
import org.hsweb.commons.StringUtils;
import org.hsweb.ezorm.core.NestConditional;
import org.hsweb.ezorm.core.Query;
import org.hsweb.ezorm.core.SimpleNestConditional;
import org.hsweb.ezorm.core.param.QueryParam;
import org.hsweb.ezorm.es.ESQuery;
import org.hsweb.ezorm.es.ESTable;

import java.sql.SQLException;
import java.util.List;

/**
 * @author zhouhao
 */
class SimpleESQuery<T> implements ESQuery<T> {
    Accepter<Query<T>,Object> accepter   = this::and;
    QueryParam         queryParam = null;
    ESTable<T>         table      = null;
    RestClient         client     = null;

    public SimpleESQuery(ESTable<T> table) {
        this.table = table;
    }

    public String getURI() {
        return StringUtils.concat("/", table.getMeta().getDatabaseMetaData().getName(), "/", table.getMeta().getAlias(), "/");
    }

    public Query<T> skipTrigger() {
        return this;
    }

    @Override
    public SimpleESQuery<T> setParam(QueryParam param) {
        this.queryParam = param;
        return this;
    }

    @Override
    public SimpleESQuery<T> select(String... columns) {
        queryParam.includes(columns);
        return this;
    }

    @Override
    public SimpleESQuery<T> selectExcludes(String... columns) {
        queryParam.excludes(columns);
        return this;
    }

    @Override
    public SimpleESQuery<T> orderByAsc(String column) {
        queryParam.orderBy(column).asc();
        return this;
    }

    @Override
    public SimpleESQuery<T> orderByDesc(String column) {
        queryParam.orderBy(column).desc();
        return this;
    }

    @Override
    public List<T> list() throws SQLException {
        // TODO: 16-11-3
        return null;
    }

    @Override
    public List<T> list(int pageIndex, int pageSize) throws SQLException {
        // TODO: 16-11-3
        return null;
    }

    @Override
    public T single() throws SQLException {
        // TODO: 16-11-3  
        return null;
    }

    @Override
    public int total() throws SQLException {
        // TODO: 16-11-3
        return 0;
    }

    @Override
    public NestConditional<Query<T>> nest() {
        return new SimpleNestConditional<>(this, this.queryParam.nest());
    }

    @Override
    public NestConditional<Query<T>> nest(String column, Object value) {
        return new SimpleNestConditional<>(this, this.queryParam.nest(column, value));
    }

    @Override
    public NestConditional<Query<T>> orNest() {
        return new SimpleNestConditional<>(this, this.queryParam.orNest());
    }

    @Override
    public NestConditional<Query<T>> orNest(String column, Object value) {
        return new SimpleNestConditional<>(this, this.queryParam.orNest(column, value));
    }

    @Override
    public Query<T> and() {
        accepter = this::and;
        return this;
    }

    @Override
    public Query<T> or() {
        accepter = this::or;
        return this;
    }

    @Override
    public Query<T> and(String column, String termType, Object value) {
        queryParam.and(column, termType, value);
        return this;
    }

    @Override
    public Query<T> or(String column, String termType, Object value) {
        queryParam.or(column, termType, value);
        return this;
    }

    @Override
    public Accepter<Query<T>,Object> getAccepter() {
        return accepter;
    }

    @Override
    public Query<T> sql(String sql, Object... params) {
        return null;
    }
}
