package org.hswebframework.ezorm.rdb.mapping.defaults.record;

import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.ReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.SyncRepository;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.supports.h2.H2ConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.h2.H2R2dbcConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.Date;

import static org.junit.Assert.*;

public class RecordReactiveRepositoryTest {

    private ReactiveRepository<Record, String> repository;

    @Before
    public void init() {
        RDBDatabaseMetadata databaseMetadata = new RDBDatabaseMetadata(Dialect.H2);

        H2SchemaMetadata h2 = new H2SchemaMetadata("PUBLIC");
        databaseMetadata.setCurrentSchema(h2);
        databaseMetadata.addSchema(h2);
        ReactiveSqlExecutor sqlExecutor = new TestReactiveSqlExecutor(new H2R2dbcConnectionProvider());

        databaseMetadata.addFeature(sqlExecutor);
        databaseMetadata.addFeature(ReactiveSyncSqlExecutor.of(sqlExecutor));
        DefaultDatabaseOperator operator = DefaultDatabaseOperator.of(databaseMetadata);
        operator.ddl()
                .createOrAlter("record_test")
                .addColumn("id").primaryKey().varchar(32).commit()
                .addColumn("name").varchar(32).commit()
                .addColumn("state").number(4).commit()
                .addColumn("create_time").alias("createTime").datetime().commit()
                .commit().sync();

        repository = operator.dml().createReactiveRepository("record_test");
    }

    @Test
    public void testCrud() {
        repository.newInstance()
                .map(record -> record.putValue("id", "test")
                        .putValue("name", "test")
                        .putValue("state", 1)
                        .putValue("create_time", new Date()))
                .as(repository::insert)
                .as(StepVerifier::create)
                .expectNext(1)
                .verifyComplete();

        repository.findById(Mono.just("test"))
                .map(r -> r.getObject("name"))
                .as(StepVerifier::create)
                .expectNext("test")
                .verifyComplete();

        repository.createUpdate()
                .set("state", 2)
                .where("id", "test")
                .execute()
                .as(StepVerifier::create)
                .expectNext(1)
                .verifyComplete();

        repository.deleteById(Mono.just("test"))
                .as(StepVerifier::create)
                .expectNext(1)
                .verifyComplete();
    }
}