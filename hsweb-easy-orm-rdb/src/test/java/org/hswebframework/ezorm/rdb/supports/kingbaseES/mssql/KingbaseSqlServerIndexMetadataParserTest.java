package org.hswebframework.ezorm.rdb.supports.kingbaseES.mssql;

import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.junit.Assert;
import org.junit.Test;
import reactor.test.StepVerifier;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class KingbaseSqlServerIndexMetadataParserTest {

    @Test
    public void shouldParseKingbaseFlagsAndCompositeIndexOrder() {
        KingbaseSqlServerSchemaMetadata schema = new KingbaseSqlServerSchemaMetadata("dbo");
        KingbaseSqlServerCatalogExecutor executor = executor();
        schema.addFeature(executor);
        KingbaseSqlServerIndexMetadataParser parser = new KingbaseSqlServerIndexMetadataParser(schema);
        assertIndexes(parser.parseTableIndex("account"));
        for (org.hswebframework.ezorm.rdb.executor.SqlRequest request : executor.requests) {
            Assert.assertArrayEquals(new Object[]{"dbo", "account", "%%"}, request.getParameters());
        }
    }

    @Test
    public void shouldParseIndexesReactivelyWithTheSameFixedContract() {
        KingbaseSqlServerSchemaMetadata schema = new KingbaseSqlServerSchemaMetadata("dbo");
        schema.addFeature(executor().reactive());
        new KingbaseSqlServerIndexMetadataParser(schema).parseTableIndexReactive("account").collectList()
            .as(StepVerifier::create).assertNext(KingbaseSqlServerIndexMetadataParserTest::assertIndexes).verifyComplete();
    }

    @Test
    public void shouldBindIndexNameAndSchemaForSyncAndReactiveLookup() {
        KingbaseSqlServerSchemaMetadata schema = new KingbaseSqlServerSchemaMetadata("tenant_a");
        KingbaseSqlServerCatalogExecutor executor = new KingbaseSqlServerCatalogExecutor(sql -> Collections.emptyList());
        schema.addFeature(executor);
        schema.addFeature(executor.reactive());
        KingbaseSqlServerIndexMetadataParser parser = new KingbaseSqlServerIndexMetadataParser(schema);
        Assert.assertFalse(parser.parseByName("idx_account_name").isPresent());
        parser.parseByNameReactive("idx_account_name").as(StepVerifier::create).verifyComplete();
        Assert.assertEquals(2, executor.requests.size());
        for (org.hswebframework.ezorm.rdb.executor.SqlRequest request : executor.requests) {
            Assert.assertArrayEquals(new Object[]{"tenant_a", "%%", "idx_account_name"}, request.getParameters());
            Assert.assertTrue(request.getSql().contains("t.schema_id = SCHEMA_ID(?)"));
            Assert.assertTrue(request.getSql().contains("i.name LIKE ?"));
        }
    }

    private static KingbaseSqlServerCatalogExecutor executor() {
        return new KingbaseSqlServerCatalogExecutor(sql -> {
            // Mix the boolean representations accepted by Kingbase drivers.
            return Arrays.asList(
                Map.of("index_name", "account_pkey", "table_name", "account", "is_unique", true,
                    "primary_key", "1", "column_name", "id", "key_ordinal", 1, "is_desc", 0),
                Map.of("index_name", "idx_account_name", "table_name", "account", "is_unique", "1",
                    "primary_key", false, "column_name", "name", "key_ordinal", "1", "is_desc", 0),
                Map.of("index_name", "idx_account_name", "table_name", "account", "is_unique", "t",
                    "primary_key", 0, "column_name", "age", "key_ordinal", 2, "is_desc", 1));
        });
    }

    private static void assertIndexes(List<RDBIndexMetadata> indexes) {
        Assert.assertEquals(2, indexes.size());
        Map<String, RDBIndexMetadata> byName = indexes.stream()
            .collect(Collectors.toMap(RDBIndexMetadata::getName, index -> index));
        Assert.assertTrue(byName.get("account_pkey").isPrimaryKey());
        Assert.assertTrue(byName.get("account_pkey").isUnique());
        RDBIndexMetadata composite = byName.get("idx_account_name");
        Assert.assertEquals("account", composite.getTableName());
        Assert.assertTrue(composite.isUnique());
        Assert.assertFalse(composite.isPrimaryKey());
        Assert.assertEquals(2, composite.getColumns().size());
        Assert.assertEquals("name", composite.getColumns().get(0).getColumn());
        Assert.assertEquals(1, composite.getColumns().get(0).getSortIndex());
        Assert.assertEquals(RDBIndexMetadata.IndexSort.asc, composite.getColumns().get(0).getSort());
        Assert.assertEquals("age", composite.getColumns().get(1).getColumn());
        Assert.assertEquals(2, composite.getColumns().get(1).getSortIndex());
        Assert.assertEquals(RDBIndexMetadata.IndexSort.desc, composite.getColumns().get(1).getSort());
    }
}
