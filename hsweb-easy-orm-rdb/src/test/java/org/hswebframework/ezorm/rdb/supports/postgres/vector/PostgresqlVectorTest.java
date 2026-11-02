package org.hswebframework.ezorm.rdb.supports.postgres.vector;

import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.core.DefaultValueGenerator;
import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.core.meta.ObjectMetadata;
import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.mapping.SyncRepository;
import org.hswebframework.ezorm.rdb.mapping.annotation.ColumnType;
import org.hswebframework.ezorm.rdb.mapping.defaults.DefaultSyncRepository;
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

import javax.persistence.Column;
import javax.persistence.Id;
import javax.persistence.Table;
import java.util.UUID;

@Slf4j
public class PostgresqlVectorTest {
    protected RDBSchemaMetadata getSchema() {
        return new PostgresqlSchemaMetadata("public");
    }

    protected Dialect getDialect() {
        return Dialect.POSTGRES;
    }

    protected SyncSqlExecutor getSqlExecutor() {
        return new TestSyncSqlExecutor(new PostgresqlVectorConnectionProvider());
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
            public RuntimeDefaultValue generate(ObjectMetadata meta) {
                return () -> UUID.randomUUID().toString().replace("-", "");
            }

            @Override
            public String getName() {
                return "UUID";
            }
        });
        log.debug(schema.toString());

        metadata.setCurrentSchema(schema);
        metadata.addSchema(schema);
        metadata.addFeature(getSqlExecutor());

        return metadata;
    }

    @Test
    public void testVectorField() {
        RDBDatabaseMetadata database = getDatabase();
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);
        try {
            JpaEntityTableMetadataParser parser = new JpaEntityTableMetadataParser();
            parser.setDatabaseMetadata(database);

            RDBTableMetadata table = parser
                .parseTableMetadata(BasicVectorEntity.class)
                .orElseThrow(NullPointerException::new);

            operator.ddl()
                    .createOrAlter(table)
                    .commit()
                    .sync();

            EntityResultWrapper<BasicVectorEntity> wrapper = new EntityResultWrapper<>(BasicVectorEntity::new);
            wrapper.setMapping(table
                                   .<EntityColumnMapping>getFeature(MappingFeatureType.columnPropertyMapping.createFeatureId(BasicVectorEntity.class))
                                   .orElseThrow(NullPointerException::new));

            SyncRepository<BasicVectorEntity, String> repository =
                new DefaultSyncRepository<>(operator, table, BasicVectorEntity.class, wrapper);

            BasicVectorEntity entity = new BasicVectorEntity();
            entity.setId("vec-sync");
            entity.setName("sync-test");
            entity.setEmbed(new Float[]{1F, 2F, 3F});

            repository.insert(entity);

            {
                BasicVectorEntity loaded = repository.findById("vec-sync").orElseThrow(NullPointerException::new);
                Assert.assertArrayEquals(new Float[]{1F, 2F, 3F}, loaded.getEmbed());
            }

            {
                BasicVectorEntity loaded = repository
                    .createQuery()
                    .and(PostgresqlVectorTest.BasicVectorEntity::getEmbed,
                         VectorTermType.vector_l2.name(),
                         new Float[]{1F, 2F, 3F})
                    .fetchOne()
                    .orElseThrow(NullPointerException::new);
                Assert.assertArrayEquals(new Float[]{1F, 2F, 3F}, loaded.getEmbed());
            }

            {
                BasicVectorEntity loaded = repository
                    .createQuery()
                    .and(PostgresqlVectorTest.BasicVectorEntity::getEmbed,
                         VectorTermType.vector_cos.name(),
                         new Float[]{1F, 2F, 3F})
                    .fetchOne()
                    .orElseThrow(NullPointerException::new);
                Assert.assertArrayEquals(new Float[]{1F, 2F, 3F}, loaded.getEmbed());
            }

            {
                BasicVectorEntity loaded = repository
                    .createQuery()
                    .and(PostgresqlVectorTest.BasicVectorEntity::getEmbed,
                         VectorTermType.vector_ip.name(),
                         new Float[]{1F, 2F, 3F})
                    .fetchOne()
                    .orElseThrow(NullPointerException::new);
                Assert.assertArrayEquals(new Float[]{1F, 2F, 3F}, loaded.getEmbed());
            }


        } finally {
            try {
                getSqlExecutor().execute(org.hswebframework.ezorm.rdb.executor.SqlRequests.of("drop table test_vector_basic"));
            } catch (Exception ignore) {
            }
        }
    }

    @Setter
    @Getter
    @Table(name = "test_vector_basic")
    public static class BasicVectorEntity {
        @Id
        @Column(length = 32)
        private String id;

        @Column(length = 64, nullable = false)
        private String name;

        @Column(length = 3, nullable = false)
        @ColumnType(typeId = "vector")
        private Float[] embed;

    }
}
