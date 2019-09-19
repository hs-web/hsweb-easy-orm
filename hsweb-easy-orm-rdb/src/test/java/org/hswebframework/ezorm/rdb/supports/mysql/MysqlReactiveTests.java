package org.hswebframework.ezorm.rdb.supports.mysql;

import io.r2dbc.spi.Connection;
import io.r2dbc.spi.Result;
import io.r2dbc.spi.Statement;
import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.BasicReactiveTests;
import org.hswebframework.ezorm.rdb.supports.posgres.PostgresqlSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlR2dbcConnectionProvider;
import org.junit.Ignore;
import org.junit.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;
import reactor.test.StepVerifier;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Date;

import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.*;

@Ignore
public class MysqlReactiveTests extends BasicReactiveTests {
    @Override
    protected RDBSchemaMetadata getSchema() {
        return new PostgresqlSchemaMetadata("ezorm");
    }

    @Override
    protected Dialect getDialect() {
        return Dialect.MYSQL;
    }

    @Override
    protected ReactiveSqlExecutor getSqlExecutor() {

        return new TestReactiveSqlExecutor("$", 0, new MysqlR2dbcConnectionProvider()) {
            @Override
            protected void releaseConnection(SignalType type, Connection connection) {
                super.releaseConnection(type, connection);
            }

            @Override
            protected Flux<Result> doExecute(Flux<SqlRequest> sqlRequestFlux) {

                return sqlRequestFlux
                        .zipWith(this.getConnection())
                        .flatMap((tuple2) -> doExecute(tuple2.getT2(), tuple2.getT1())
                                .doFinally(si -> releaseConnection(si, tuple2.getT2())));
            }

            @Override
            protected void bindNull(Statement statement, int index, Class type) {
                statement.bindNull(index, type);
            }

            @Override
            protected void bind(Statement statement, int index, Object value) {
                if (value instanceof LocalDateTime) {
                    value = Date.from(((LocalDateTime) value).atZone(ZoneOffset.UTC).toInstant());
                }
                statement.bind(index, value);
            }

            @Override
            protected SqlRequest convertRequest(SqlRequest sqlRequest) {
                return sqlRequest;
            }
        };
    }

}
