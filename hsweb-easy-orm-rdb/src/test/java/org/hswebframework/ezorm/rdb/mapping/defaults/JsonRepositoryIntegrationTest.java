package org.hswebframework.ezorm.rdb.mapping.defaults;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.rdb.ConnectionProvider;
import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.codec.JsonValueCodec;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.mapping.annotation.JsonCodec;
import org.hswebframework.ezorm.rdb.mapping.annotation.ColumnType;
import org.hswebframework.ezorm.rdb.mapping.jpa.JpaEntityTableMetadataParser;
import org.hswebframework.ezorm.rdb.mapping.parser.DefaultValueCodecResolver;
import org.hswebframework.ezorm.rdb.mapping.wrapper.EntityResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.jetlinks.json.JsonMapper;
import org.jetlinks.json.jackson2.Jackson2Provider;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import javax.persistence.Column;
import javax.persistence.Id;
import javax.persistence.Lob;
import javax.persistence.Table;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.JDBCType;
import java.sql.DriverManager;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.Assert.*;

public class JsonRepositoryIntegrationTest {

    private Connection connection;
    private RDBDatabaseMetadata metadata;
    private H2SchemaMetadata schema;
    private DefaultDatabaseOperator operator;

    @Before
    public void init() throws Exception {
        connection = DriverManager.getConnection("jdbc:h2:mem:json_" + UUID.randomUUID(), "sa", "");
        metadata = new RDBDatabaseMetadata(Dialect.H2);
        schema = new H2SchemaMetadata("PUBLIC");
        metadata.setCurrentSchema(schema);
        metadata.addSchema(schema);
        metadata.addFeature(new TestSyncSqlExecutor(new ConnectionProvider() {
            @Override public Connection getConnection() { return connection; }
            @Override public void releaseConnect(Connection ignored) { /* 夹具统一关闭连接。 */ }
        }));
        operator = DefaultDatabaseOperator.of(metadata);
        operator.ddl().createOrAlter("json_entity")
            .addColumn("id").primaryKey().varchar(32).commit()
            .addColumn("payload").clob().commit()
            .addColumn("optional_json").alias("optional").varchar(4096).commit()
            .commit().sync();
    }

    @After
    public void close() throws Exception {
        if (connection != null) {
            connection.close();
        }
    }

    @Test
    public void defaultJsonColumnsRetainNestedTypesAndValues() throws Exception {
        roundTrip(null, "displayName");
    }

    @Test
    public void explicitFacadeUsesExistingResolverExtension() throws Exception {
        JsonMapper mapper = Jackson2Provider.adapt(JsonValueCodec.defaultMapper.copy()
            .setPropertyNamingStrategy(com.fasterxml.jackson.databind.PropertyNamingStrategies.SNAKE_CASE));
        roundTrip(mapper, "display_name");
    }

    private void roundTrip(JsonMapper mapper, String storedName) throws Exception {
        JpaEntityTableMetadataParser parser = new JpaEntityTableMetadataParser();
        parser.setDatabaseMetadata(metadata);
        if (mapper != null) {
            DefaultValueCodecResolver custom = new DefaultValueCodecResolver();
            custom.register(JsonCodec.class, (field, annotation) -> JsonValueCodec.ofField(field.getField(), mapper));
            parser.setValueCodecResolver(field -> custom.resolve(field)
                .or(() -> DefaultValueCodecResolver.COMMONS.resolve(field)));
        }
        RDBTableMetadata table = parser.parseTableMetadata(JsonEntity.class).orElseThrow();
        schema.addTable(table);
        EntityResultWrapper<JsonEntity> wrapper = new EntityResultWrapper<>(JsonEntity::new);
        wrapper.setMapping(table.<EntityColumnMapping>getFeature(
            MappingFeatureType.columnPropertyMapping.createFeatureId(JsonEntity.class)).orElseThrow());
        var repository = new DefaultSyncRepository<JsonEntity, String>(operator, table, JsonEntity.class, wrapper);

        Item item = new Item();
        item.setDisplayName("中文😀");
        item.setPrice(new BigDecimal("12345678901234567890.123456789"));
        item.setSequence(9007199254740993L);
        item.setCreatedAt(new Date(1700000000123L));
        JsonEntity entity = new JsonEntity();
        entity.setId("test");
        entity.setPayload(Map.of("items", List.of(item)));
        entity.setOptional(item);
        repository.insert(entity);
        assertEquals(entity, repository.findById("test").orElseThrow());
        try (var query = connection.prepareStatement("SELECT payload FROM json_entity WHERE id='test'");
             var rows = query.executeQuery()) {
            assertTrue(rows.next());
            assertTrue(rows.getString(1).contains("\"" + storedName + "\""));
        }

        assertEquals(1, repository.createUpdate().set("payload", Map.of())
            .setNull("optional").where("id", "test").execute());
        JsonEntity empty = repository.findById("test").orElseThrow();
        assertEquals(Map.of(), empty.getPayload());
        assertNull(empty.getOptional());

        // 区分 SQL NULL 和合法 JSON null，两者都应映射为空属性。
        try (var update = connection.prepareStatement("UPDATE json_entity SET payload='null' WHERE id='test'")) {
            assertEquals(1, update.executeUpdate());
        }
        assertNull(repository.findById("test").orElseThrow().getPayload());
    }

    @Getter
    @Setter
    @EqualsAndHashCode
    @Table(name = "json_entity")
    public static class JsonEntity {
        @Id
        @Column
        private String id;
        @Lob
        @JsonCodec
        @ColumnType(javaType = String.class, jdbcType = JDBCType.CLOB)
        @Column
        private Map<String, List<Item>> payload;
        @JsonCodec
        @ColumnType(javaType = String.class, jdbcType = JDBCType.VARCHAR)
        @Column(name = "optional_json")
        private Item optional;
    }

    @Getter
    @Setter
    @EqualsAndHashCode
    public static class Item {
        private String displayName;
        private BigDecimal price;
        private long sequence;
        private Date createdAt;
        private String missing;
    }
}
