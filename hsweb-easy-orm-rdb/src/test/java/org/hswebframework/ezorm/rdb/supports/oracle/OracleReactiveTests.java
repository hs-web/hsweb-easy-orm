package org.hswebframework.ezorm.rdb.supports.oracle;

import io.r2dbc.spi.Statement;
import org.hswebframework.ezorm.rdb.TestJdbcReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.exception.DuplicateKeyException;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.BasicReactiveTests;
import org.hswebframework.ezorm.rdb.supports.BasicTestEntity;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlR2dbcConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlSchemaMetadata;
import org.junit.Ignore;
import org.junit.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Date;

public class OracleReactiveTests extends BasicReactiveTests {
    @Override
    protected RDBSchemaMetadata getSchema() {
        return new OracleSchemaMetadata("SYSTEM");
    }

    @Override
    protected Dialect getDialect() {
        return Dialect.ORACLE;
    }

    @Override
    protected ReactiveSqlExecutor getSqlExecutor() {

        return new TestJdbcReactiveSqlExecutor(new OracleConnectionProvider());

//        return new TestReactiveSqlExecutor(":",new OracleR2dbcConnectionProvider()){
//            @Override
//            protected void bindNull(Statement statement, int index, Class type) {
//                if (type == Date.class) {
//                    type = LocalDateTime.class;
//                }
//                statement.bindNull(index, type);
//            }
//
//            @Override
//            protected void bind(Statement statement, int index, Object value) {
//                if (value instanceof Date) {
//                    value = ((Date) value)
//                            .toInstant()
//                            .atZone(ZoneOffset.systemDefault())
//                            .toLocalDateTime();
//                }
//                statement.bind(index, value);
//            }
//        };

    }
}
