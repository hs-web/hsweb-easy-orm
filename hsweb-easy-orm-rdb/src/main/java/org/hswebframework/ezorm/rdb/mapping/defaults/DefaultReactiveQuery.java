package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.ReactiveQuery;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.DMLOperator;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.*;
import static org.hswebframework.ezorm.rdb.operator.dml.query.Selects.count1;

public class DefaultReactiveQuery<T> extends DefaultQuery<T, ReactiveQuery<T>> implements ReactiveQuery<T> {

    public DefaultReactiveQuery(TableOrViewMetadata tableMetadata, Class<T> entityType, DMLOperator operator, ResultWrapper<T, ?> wrapper) {
        super(tableMetadata, entityType, operator, wrapper);
    }

    @Override
    public Flux<T> fetch() {
        return operator
                .query(tableName)
                .select(getSelectColumn())
                .where(param.getTerms())
                .orderBy(getSortOrder())
                .when(param.isPaging(), query -> query.paging(param.getPageIndex(), param.getPageSize()))
                .fetch(wrapper)
                .reactive();
    }

    @Override
    public Mono<T> fetchOne() {
        return operator
                .query(tableName)
                .select(getSelectColumn())
                .where(param.getTerms())
                .orderBy(getSortOrder())
                .paging(0, 1)
                .fetch(wrapper)
                .reactive()
                .last();
    }

    @Override
    public Mono<Integer> count() {
        return operator
                .query(tableName)
                .select(count1().as("total"))
                .where(param.getTerms())
                .fetch(column("total", Number.class::cast))
                .reactive()
                .map(Number::intValue)
                .last(0);
    }

}
