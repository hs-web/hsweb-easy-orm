package org.hswebframework.ezorm.rdb.supports.postgres.vector;

import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.core.DefaultValue;
import org.hswebframework.ezorm.core.DefaultValueGenerator;
import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.core.meta.ObjectMetadata;
import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.events.EventListener;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.mapping.ReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.defaults.DefaultReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.jpa.JpaEntityTableMetadataParser;
import org.hswebframework.ezorm.rdb.mapping.wrapper.EntityResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.postgres.VectorTermType;
import org.junit.Assert;
import org.junit.Test;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.UUID;

@Slf4j
public class PostgresqlReactiveVectorTest {
    protected RDBSchemaMetadata getSchema() {
        return new PostgresqlSchemaMetadata("public");
    }

    protected Dialect getDialect() {
        return Dialect.POSTGRES;
    }

    protected ReactiveSqlExecutor getReactiveSqlExecutor() {
        return new TestReactiveSqlExecutor(new PostgresqlR2dbcVectorConnectionProvider());
    }

    protected RDBDatabaseMetadata getDatabase() {
        RDBDatabaseMetadata metadata = new RDBDatabaseMetadata(getDialect());

        RDBSchemaMetadata schema = getSchema();
        schema.addFeature(new DefaultValueGenerator() {
            @Override
            public String getSortId() {
                return "uuid";
            }

            @Override
            public DefaultValue generate(ObjectMetadata meta) {
                return (RuntimeDefaultValue) () -> UUID.randomUUID().toString().replace("-", "");
            }

            @Override
            public String getName() {
                return "UUID";
            }
        });

        schema.addFeature((EventListener) (type, context) -> System.out.println(type));
        log.debug(schema.toString());

        metadata.setCurrentSchema(schema);
        metadata.addSchema(schema);
        ReactiveSqlExecutor sqlExecutor = getReactiveSqlExecutor();

        metadata.addFeature(sqlExecutor);
        metadata.addFeature(ReactiveSyncSqlExecutor.of(sqlExecutor));

        return metadata;
    }

    @Test
    public void testReactiveVectorField() {
        RDBDatabaseMetadata database = getDatabase();
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);
        try {
            JpaEntityTableMetadataParser parser = new JpaEntityTableMetadataParser();
            parser.setDatabaseMetadata(database);

            RDBTableMetadata table = parser
                .parseTableMetadata(PostgresqlVectorTest.BasicVectorEntity.class)
                .orElseThrow(NullPointerException::new);

            operator.ddl()
                    .createOrAlter(table)
                    .commit()
                    .reactive()
                    .block();

            EntityResultWrapper<PostgresqlVectorTest.BasicVectorEntity> wrapper = new EntityResultWrapper<>(PostgresqlVectorTest.BasicVectorEntity::new);
            wrapper.setMapping(table
                                   .<EntityColumnMapping>getFeature(MappingFeatureType.columnPropertyMapping.createFeatureId(PostgresqlVectorTest.BasicVectorEntity.class))
                                   .orElseThrow(NullPointerException::new));

            ReactiveRepository<PostgresqlVectorTest.BasicVectorEntity, String> repository =
                new DefaultReactiveRepository<>(operator, table, PostgresqlVectorTest.BasicVectorEntity.class, wrapper);

            PostgresqlVectorTest.BasicVectorEntity entity = new PostgresqlVectorTest.BasicVectorEntity();
            entity.setId("vec-reactive");
            entity.setName("reactive-test");
            entity.setEmbed(new Float[]{1F, 2F, 3F});

            repository.insert(Mono.just(entity))
                      .as(StepVerifier::create)
                      .expectNext(1)
                      .verifyComplete();

            repository.findById(Mono.just("vec-reactive"))
                      .as(StepVerifier::create)
                      .assertNext(loaded -> Assert.assertArrayEquals(new Float[]{1F, 2F, 3F}, loaded.getEmbed()))
                      .verifyComplete();

            repository
                .createQuery()
                .and(PostgresqlVectorTest.BasicVectorEntity::getEmbed,
                     VectorTermType.vector_l2.name(),
                     new Float[]{1F, 2F, 3F})
                .fetch()
                .as(StepVerifier::create)
                .assertNext(loaded -> Assert.assertArrayEquals(new Float[]{1F, 2F, 3F}, loaded.getEmbed()))
                .verifyComplete();

            repository
                .createQuery()
                .and(PostgresqlVectorTest.BasicVectorEntity::getEmbed,
                     VectorTermType.vector_cos.name(),
                     new Float[]{1F, 2F, 3F})
                .fetch()
                .as(StepVerifier::create)
                .assertNext(loaded -> Assert.assertArrayEquals(new Float[]{1F, 2F, 3F}, loaded.getEmbed()))
                .verifyComplete();

            repository
                .createQuery()
                .and(PostgresqlVectorTest.BasicVectorEntity::getEmbed,
                     VectorTermType.vector_ip.name(),
                     new Float[]{1F, 2F, 3F})
                .fetch()
                .as(StepVerifier::create)
                .assertNext(loaded -> Assert.assertArrayEquals(new Float[]{1F, 2F, 3F}, loaded.getEmbed()))
                .verifyComplete();

        } finally {
            try {
                getReactiveSqlExecutor().execute(Mono.just(SqlRequests.of("drop table test_vector_basic"))).block();
            } catch (Exception ignore) {
            }
        }
    }

}
