package org.hswebframework.ezorm.rdb.mapping;

import org.hswebframework.ezorm.core.NestConditional;
import org.hswebframework.ezorm.core.SimpleNestConditional;
import org.hswebframework.ezorm.core.dsl.Delete;
import org.hswebframework.ezorm.core.mapping.SyncQuery;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.mapping.wrapper.EntityResultWrapper;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.dml.query.Selects;

import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;

import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.*;

public class RDBSyncQuery<T> implements SyncQuery<T> {

    private QueryParam param = new QueryParam();

    private Accepter<SyncQuery<T>, Object> accepter = this::and;

    private DatabaseOperator operator;

    private Class<T> entityType;

    private String tableName;

    private Supplier<T> entitySupplier;

    private ResultWrapper<T, ?> wrapper;

    public SyncQuery<T> paging(int pageIndex, int pageSize) {
        param.doPaging(pageIndex, pageSize);
        return this;
    }

    @Override
    public List<T> fetch() {
        return operator.dml()
                .query()
                .from(tableName)
                .where(param.getTerms())
                .when(param.isPaging(), operator -> operator.paging(param.getPageIndex(), param.getPageSize()))
                .fetch(list(wrapper))
                .sync();
    }

    @Override
    public Optional<T> fetchOne() {
        return operator.dml()
                .query()
                .from(tableName)
                .where(param.getTerms())
                .paging(0, 1)
                .fetch(optional(single(wrapper)))
                .sync();
    }

    @Override
    public int count() {
        return operator.dml()
                .query()
                .select(Selects.count1().as("total"))
                .from(tableName)
                .where(param.getTerms())
                .fetch(optional(single(column("total", Number.class::cast))))
                .sync()
                .map(Number::intValue)
                .orElse(0);
    }

    @Override
    public NestConditional<SyncQuery<T>> nest() {
        return new SimpleNestConditional<>(this, param.nest());
    }

    @Override
    public NestConditional<SyncQuery<T>> orNest() {

        return new SimpleNestConditional<>(this, param.orNest());
    }

    @Override
    public SyncQuery<T> and() {
        accepter = this::and;
        return this;
    }

    @Override
    public SyncQuery<T> or() {
        accepter = this::or;
        return this;
    }

    @Override
    public SyncQuery<T> and(String column, String termType, Object value) {
        return param.and(column, termType, value);
    }

    @Override
    public SyncQuery<T> or(String column, String termType, Object value) {
        return param.or(column, termType, value);
    }

    @Override
    public SyncQuery<T> sql(String sql, Object... params) {
        return null;
    }

    @Override
    public Accepter<SyncQuery<T>, Object> getAccepter() {
        return accepter;
    }

    @Override
    public SyncQuery<T> accept(Term term) {
        param.getTerms().add(term);

        return this;
    }
}
