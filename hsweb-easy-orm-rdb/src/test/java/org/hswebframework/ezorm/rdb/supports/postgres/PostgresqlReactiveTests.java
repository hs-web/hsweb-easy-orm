package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.exception.DuplicateKeyException;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.supports.BasicReactiveTests;
import org.hswebframework.ezorm.rdb.supports.BasicTestEntity;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedHashMap;

public class PostgresqlReactiveTests extends BasicReactiveTests {
    @Override
    protected RDBSchemaMetadata getSchema() {
        return new PostgresqlSchemaMetadata("public");
    }

    @Override
    protected Dialect getDialect() {
        return Dialect.POSTGRES;
    }

    @Override
    protected ReactiveSqlExecutor getSqlExecutor() {

        return new TestReactiveSqlExecutor(new PostgresqlR2dbcConnectionProvider());
    }

    String jsonbTableName = "test_jsonb_exist_sync";

    @Test
    public void testBatchInsertId() {
        getSqlExecutor()
            .execute(SqlRequests.of("create table test_ser_table (id serial primary key,name varchar)"))
            .block();
        DatabaseOperator operator = DefaultDatabaseOperator.of(getDatabase());
        operator.dml()
                .upsert("test_ser_table")
                .columns("name")
                .values("test")
                .values("test2")
                .execute()
                .block();

        operator.dml()
                .query("test_ser_table")
                .fetch(ResultWrappers.map())
                .reactive()
                .doOnNext(System.out::println)
                .as(StepVerifier::create)
                .expectNextCount(2);

    }

    @Test
    public void testException() {
        repository.insert(Mono.just(BasicTestEntity
                                        .builder()
                                        .name("test")
                                        .id("test")
                                        .state((byte) 1)
                                        .build()))
                  .as(StepVerifier::create)
                  .expectNext(1)
                  .verifyComplete();

        repository.insert(Mono.just(BasicTestEntity
                                        .builder()
                                        .name("test")
                                        .id("test")
                                        .state((byte) 1)
                                        .build()))
                  .as(StepVerifier::create)
                  .expectError(DuplicateKeyException.class)
                  .verify();
    }

    @Test
    public void testJsonbExistTerm() {
        RDBDatabaseMetadata database = getDatabase();
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);

        try {

            operator.sql()
                    .reactive()
                    .execute(SqlRequests.of("insert into public." + jsonbTableName + " (id,data) values (?, ?::jsonb)",
                                            "1",
                                            "{\"name\":\"JetLinks\",\"age\":18}"))
                    .block();
            operator.sql()
                    .reactive()
                    .execute(SqlRequests.of("insert into public." + jsonbTableName + " (id,data) values (?, ?::jsonb)",
                                            "2",
                                            "{\"name\":\"Other\"}"))
                    .block();
            operator.sql()
                    .reactive()
                    .execute(SqlRequests.of("insert into public." + jsonbTableName + " (id,data) values (?, ?::jsonb)",
                                            "3",
                                            "{\"age\":20,\"status\":\"ok\"}"))
                    .block();

            operator.dml()
                    .query(jsonbTableName)
                    .select("id")
                    .where(q -> q.where("data$exist", "name"))
                    .fetch(ResultWrappers.map())
                    .reactive()
                    .map(map -> String.valueOf(map.get("id")))
                    .collectList()
                    .as(StepVerifier::create)
                    .assertNext(ids -> Assert.assertEquals(Arrays.asList("1", "2"), ids))
                    .verifyComplete();

            operator.dml()
                    .query(jsonbTableName)
                    .select("id")
                    .where(q -> q.where("data$exist$any", "name,status"))
                    .fetch(ResultWrappers.map())
                    .reactive()
                    .map(map -> String.valueOf(map.get("id")))
                    .collectList()
                    .as(StepVerifier::create)
                    .assertNext(ids -> Assert.assertEquals(Arrays.asList("1", "2", "3"), ids))
                    .verifyComplete();

            operator.dml()
                    .query(jsonbTableName)
                    .select("id")
                    .where(q -> q.where("data$exist$all", Arrays.asList("name", "age")))
                    .fetch(ResultWrappers.map())
                    .reactive()
                    .map(map -> String.valueOf(map.get("id")))
                    .collectList()
                    .as(StepVerifier::create)
                    .assertNext(ids -> Assert.assertEquals(Collections.singletonList("1"), ids))
                    .verifyComplete();

            operator.dml()
                    .query(jsonbTableName)
                    .select("id")
                    .where(q -> q.where("data$exist$contains", Collections.singletonMap("name", "JetLinks")))
                    .fetch(ResultWrappers.mapStream())
                    .reactive()
                    .map(map -> String.valueOf(map.get("id")))
                    .collectList()
                    .as(StepVerifier::create)
                    .assertNext(ids -> Assert.assertEquals(Collections.singletonList("1"), ids))
                    .verifyComplete();

            operator.dml()
                    .query(jsonbTableName)
                    .select("id")
                    .where(q -> q.where("data$exist$contains", "{\"age\":20,\"status\":\"ok\"}"))
                    .fetch(ResultWrappers.mapStream())
                    .reactive()
                    .map(map -> String.valueOf(map.get("id")))
                    .collectList()
                    .as(StepVerifier::create)
                    .assertNext(ids -> Assert.assertEquals(Collections.singletonList("3"), ids))
                    .verifyComplete();

            LinkedHashMap<String, Object> containedTarget = new LinkedHashMap<>();
            containedTarget.put("name", "JetLinks");
            containedTarget.put("age", 18);
            containedTarget.put("status", "ok");

            operator.dml()
                    .query(jsonbTableName)
                    .select("id")
                    .where(q -> q.where("data$exist$contained", containedTarget))
                    .fetch(ResultWrappers.mapStream())
                    .reactive()
                    .map(map -> String.valueOf(map.get("id")))
                    .collectList()
                    .as(StepVerifier::create)
                    .assertNext(ids -> Assert.assertEquals(Collections.singletonList("1"), ids))
                    .verifyComplete();
        } finally {
            try {
                operator.sql()
                        .reactive()
                        .execute(SqlRequests.of("drop table if exists public." + jsonbTableName))
                        .block();
            } catch (Exception ignore) {
            }
        }
    }

    @Test
    @Ignore
    public void benchmark() {
        long time = System.currentTimeMillis();

        StepVerifier
            .create(repository.insertBatch(Flux.range(0, 100000)
                                               .map(integer -> BasicTestEntity.builder()
                                                                              .id("id:" + integer)
                                                                              .balance(1000L)
                                                                              .name("test:" + integer)
                                                                              .createTime(new Date())
                                                                              .state((byte) 1)
                                                                              .build())
                                               .buffer(1000)))
            .expectNext(100000)
            .verifyComplete();
        System.out.println(System.currentTimeMillis() - time);
    }

    @Before
    public void createJsonbTable() {
        RDBDatabaseMetadata database = getDatabase();
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);

        RDBTableMetadata table = database.getCurrentSchema().newTable(jsonbTableName);

        operator.ddl()
                .createOrAlter(jsonbTableName)
                .addColumn().name("id").varchar(32).primaryKey().comment("ID").commit()
                .addColumn().name("data").type(JsonbType.INSTANCE).comment("数据").commit()
                .commit()
                .sync();
    }

}
