package org.hswebframework.ezorm.spring.executor;

import io.r2dbc.spi.Connection;
import io.r2dbc.spi.ConnectionFactory;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.reactive.r2dbc.R2dbcReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.reactivestreams.Publisher;
import org.springframework.data.r2dbc.connectionfactory.ConnectionFactoryUtils;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;

@Slf4j
public abstract class TransactionR2dbcSqlExecutor extends R2dbcReactiveSqlExecutor {

    protected abstract ConnectionFactory getConnectionFactory();

    @Override
    protected Mono<Connection> getConnection() {
        return ConnectionFactoryUtils.getConnection(getConnectionFactory());
    }

    @Override
    protected void releaseConnection(SignalType type, Connection connection) {

    }

    @Override
    @Transactional(readOnly = true)
    public <E> Flux<E> select(Publisher<SqlRequest> request, ResultWrapper<E, ?> wrapper) {
        return super.select(request, wrapper);
    }

    @Override
    @Transactional
    public Mono<Integer> update(Publisher<SqlRequest> request) {
        return super.update(request);
    }

    @Override
    @Transactional(propagation = Propagation.NOT_SUPPORTED)
    public Mono<Void> execute(Publisher<SqlRequest> request) {
        return super.execute(request);
    }
}
