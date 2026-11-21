package org.hswebframework.ezorm.rdb.supports.kingbaseES.oracle;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.mapping.ReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.defaults.DefaultReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.jpa.JpaEntityTableMetadataParser;
import org.hswebframework.ezorm.rdb.mapping.wrapper.EntityResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.supports.kingbaseES.KingbaseOfficialTestDatabase;
import org.junit.Assert;
import org.junit.ClassRule;
import org.junit.Test;
import reactor.core.publisher.Flux;

import javax.persistence.Column;
import javax.persistence.Id;
import javax.persistence.Table;
import java.time.Duration;
import java.util.Arrays;
import java.util.Map;

/** Supplemental regression for the null-preserving save behavior in upstream testSaveNull. */
public class KingbaseOracleBooleanBindingTests {

    @ClassRule
    public static final KingbaseOfficialTestDatabase DATABASE = new KingbaseOfficialTestDatabase("oracle");

    @Test
    public void shouldPreserveBooleanValuesWhenSavingNull() {
        Duration timeout = Duration.ofSeconds(30);
        RDBSchemaMetadata schema = DATABASE.newSchema();
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(schema.getDialect());
        database.addSchema(schema);
        database.setCurrentSchema(schema);
        database.addFeature(DATABASE.reactive());
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);
        JpaEntityTableMetadataParser parser = new JpaEntityTableMetadataParser();
        parser.setDatabaseMetadata(database);
        RDBTableMetadata table = parser.parseTableMetadata(BooleanEntity.class).orElseThrow(AssertionError::new);
        operator.ddl().createOrAlter(table).commit().reactive().block(timeout);
        EntityResultWrapper<BooleanEntity> wrapper = new EntityResultWrapper<>(BooleanEntity::new);
        wrapper.setMapping(table.<EntityColumnMapping>getFeatureNow(
            MappingFeatureType.columnPropertyMapping.createFeatureId(BooleanEntity.class)));
        ReactiveRepository<BooleanEntity, String> repository =
            new DefaultReactiveRepository<>(operator, table, BooleanEntity.class, wrapper);

        Assert.assertEquals(Integer.valueOf(2), repository.insert(Flux.just(
            new BooleanEntity("keep-true", true), new BooleanEntity("keep-false", false))).block(timeout));
        Assert.assertEquals(5, repository.save(Arrays.asList(
            new BooleanEntity("keep-true", null), new BooleanEntity("keep-false", null),
            new BooleanEntity("new-true", true), new BooleanEntity("new-false", false),
            new BooleanEntity("new-null", null))).block(timeout).getTotal());
        Assert.assertEquals(Boolean.TRUE, repository.findById("keep-true").block(timeout).getEnabled());
        Assert.assertEquals(Boolean.FALSE, repository.findById("keep-false").block(timeout).getEnabled());
        Assert.assertEquals(Boolean.TRUE, repository.findById("new-true").block(timeout).getEnabled());
        Assert.assertEquals(Boolean.FALSE, repository.findById("new-false").block(timeout).getEnabled());
        Map<String, Object> rawNull = DATABASE.reactive().select(SqlRequests.of(
            "select \"ENABLED\" from " + table.getFullName() + " where \"ID\" = ?", "new-null"),
            ResultWrappers.lowerCase(ResultWrappers.map())).single().block(timeout);
        Assert.assertNotNull(rawNull);
        Assert.assertNull(rawNull.get("enabled"));

        Assert.assertEquals(1, repository.save(new BooleanEntity("keep-true", false)).block(timeout).getTotal());
        Assert.assertEquals(1, repository.save(new BooleanEntity("keep-true", null)).block(timeout).getTotal());
        Assert.assertEquals(Boolean.FALSE, repository.findById("keep-true").block(timeout).getEnabled());
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Table(name = "boolean_binding_test")
    public static class BooleanEntity {
        @Id
        @Column(length = 32)
        private String id;

        @Column
        private Boolean enabled;
    }
}
