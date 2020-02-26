package org.hswebframework.ezorm.rdb;

import io.r2dbc.spi.Connection;
import reactor.core.publisher.Mono;

public interface R2dbcConnectionProvider {

    Mono<Connection> getConnection();

    void releaseConnection(Connection connection);
}
