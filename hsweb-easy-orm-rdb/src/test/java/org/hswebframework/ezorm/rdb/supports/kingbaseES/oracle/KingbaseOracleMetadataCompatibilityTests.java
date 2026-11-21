package org.hswebframework.ezorm.rdb.supports.kingbaseES.oracle;

import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.supports.kingbaseES.KingbaseOfficialTestDatabase;
import org.junit.Assert;
import org.junit.ClassRule;
import org.junit.Test;

import java.time.Duration;
import java.util.List;
import java.util.Locale;

/** Native Oracle metadata contracts exercised through the Kingbase public parser APIs. */
public class KingbaseOracleMetadataCompatibilityTests {

    @ClassRule
    public static final KingbaseOfficialTestDatabase DATABASE = new KingbaseOfficialTestDatabase("oracle");

    private static final Duration TIMEOUT = Duration.ofSeconds(30);

    @Test
    public void shouldNormalizeMetadataNamesLikeNativeOracle() {
        DATABASE.sync().execute(SqlRequests.of("create table native_case_test (id int primary key, name varchar2(32))"));
        try {
            DATABASE.sync().execute(SqlRequests.of("comment on table native_case_test is 'native Oracle naming'"));
            DATABASE.sync().execute(SqlRequests.of("create index native_case_index on native_case_test (name desc)"));
            RDBSchemaMetadata schema = DATABASE.newSchema();
            schema.addFeature(DATABASE.sync());
            schema.addFeature(DATABASE.reactive());
            KingbaseOracleTableMetadataParser tables = new KingbaseOracleTableMetadataParser(schema);
            KingbaseOracleIndexMetadataParser indexes = new KingbaseOracleIndexMetadataParser(schema);
            for (String name : new String[]{"native_case_test", "NATIVE_CASE_TEST", "NaTiVe_CaSe_TeSt"}) {
                Assert.assertTrue(name, tables.tableExists(name));
                Assert.assertEquals(Boolean.TRUE, tables.tableExistsReactive(name).block(TIMEOUT));
                RDBTableMetadata sync = tables.parseByName(name).orElseThrow(AssertionError::new);
                Assert.assertEquals("native_case_test", sync.getRealName());
                Assert.assertEquals("native Oracle naming", sync.getComment());
                Assert.assertTrue(sync.getColumnNow("id").isPrimaryKey());
                RDBTableMetadata reactive = tables.parseByNameReactive(name).block(TIMEOUT);
                Assert.assertNotNull(reactive);
                Assert.assertEquals(sync.getRealName(), reactive.getRealName());
                Assert.assertEquals(2, indexes.parseTableIndex(name).size());
                Assert.assertEquals(2, indexes.parseTableIndexReactive(name).collectList().block(TIMEOUT).size());
            }
            Assert.assertTrue(indexes.parseByName("NATIVE_CASE_INDEX").isPresent());
            Assert.assertNotNull(indexes.parseByNameReactive("NaTiVe_CaSe_InDeX").block(TIMEOUT));
            schema.setName(schema.getName().toUpperCase(Locale.ROOT));
            Assert.assertTrue(tables.tableExists("native_case_test"));
            Assert.assertTrue(tables.parseAllTableName().contains("native_case_test"));
            Assert.assertTrue(tables.parseAllTableNameReactive().collectList().block(TIMEOUT).contains("native_case_test"));
            Assert.assertEquals(2, indexes.parseAll().size());
            Assert.assertEquals(2, indexes.parseAllReactive().collectList().block(TIMEOUT).size());
            Assert.assertFalse(tables.tableExists("missing_native_case_table"));
            Assert.assertFalse(indexes.parseByName("MISSING_NATIVE_CASE_INDEX").isPresent());
        } finally {
            DATABASE.sync().execute(SqlRequests.of("drop table native_case_test"));
        }
    }

    @Test
    public void shouldReadFunctionAndMixedIndexExpressionsSynchronouslyAndReactively() {
        DATABASE.sync().execute(SqlRequests.of("create table function_index_test (id int primary key, name varchar2(32), age int)"));
        try {
            DATABASE.sync().execute(SqlRequests.of("create index function_index_abs on function_index_test (abs(age))"));
            DATABASE.sync().execute(SqlRequests.of("create index function_index_mixed on function_index_test (lower(name), age desc)"));
            RDBSchemaMetadata schema = DATABASE.newSchema();
            schema.addFeature(DATABASE.sync());
            schema.addFeature(DATABASE.reactive());
            KingbaseOracleIndexMetadataParser parser = new KingbaseOracleIndexMetadataParser(schema);
            assertFunctionIndexes(parser.parseTableIndex("function_index_test"));
            assertFunctionIndexes(parser.parseTableIndexReactive("function_index_test").collectList().block(TIMEOUT));
            Assert.assertEquals("abs(age)", parser.parseByName("function_index_abs").orElseThrow(AssertionError::new)
                .getColumns().get(0).getColumn());
            Assert.assertEquals("abs(age)", parser.parseByNameReactive("function_index_abs").block(TIMEOUT)
                .getColumns().get(0).getColumn());
        } finally {
            DATABASE.sync().execute(SqlRequests.of("drop table function_index_test"));
        }
    }

    private void assertFunctionIndexes(List<RDBIndexMetadata> indexes) {
        Assert.assertNotNull(indexes);
        Assert.assertEquals(3, indexes.size());
        Assert.assertTrue(indexes.stream().anyMatch(RDBIndexMetadata::isPrimaryKey));
        RDBIndexMetadata abs = indexes.stream().filter(index -> "function_index_abs".equals(index.getName()))
            .findFirst().orElseThrow(AssertionError::new);
        Assert.assertEquals(1, abs.getColumns().size());
        Assert.assertEquals("abs(age)", abs.getColumns().get(0).getColumn());
        RDBIndexMetadata mixed = indexes.stream().filter(index -> "function_index_mixed".equals(index.getName()))
            .findFirst().orElseThrow(AssertionError::new);
        Assert.assertEquals(2, mixed.getColumns().size());
        RDBIndexMetadata.IndexColumn expression = mixed.getColumns().stream()
            .filter(column -> column.getSortIndex() == 1).findFirst().orElseThrow(AssertionError::new);
        Assert.assertTrue(expression.getColumn(), expression.getColumn().startsWith("lower("));
        Assert.assertTrue(expression.getColumn(), expression.getColumn().contains("name"));
        RDBIndexMetadata.IndexColumn age = mixed.getColumns().stream()
            .filter(column -> column.getSortIndex() == 2).findFirst().orElseThrow(AssertionError::new);
        Assert.assertEquals("age", age.getColumn());
        Assert.assertEquals(RDBIndexMetadata.IndexSort.desc, age.getSort());
    }
}
