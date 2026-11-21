package org.hswebframework.ezorm.rdb.supports.kingbaseES.mssql;

import org.hswebframework.ezorm.rdb.exception.DuplicateKeyException;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.supports.kingbaseES.KingbaseOfficialTestDatabase;
import org.junit.Assert;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Test;
import reactor.test.StepVerifier;

import java.sql.JDBCType;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/** Additional real-database checks for differences discovered by the unchanged native test contracts. */
public class KingbaseSqlServerR2dbcIntegrationTests {

    @ClassRule
    public static final KingbaseOfficialTestDatabase DATABASE = new KingbaseOfficialTestDatabase("sqlserver");

    private RDBSchemaMetadata schema;
    private SyncSqlExecutor executor;
    private DatabaseOperator operator;
    private KingbaseSqlServerTableMetadataParser parser;

    @Before
    public void init() {
        schema = DATABASE.newSchema();
        executor = DATABASE.sync();
        schema.addFeature(executor);
        schema.addFeature(DATABASE.reactive());
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(schema.getDialect());
        database.addSchema(schema);
        database.setCurrentSchema(schema);
        operator = DefaultDatabaseOperator.of(database);
        parser = new KingbaseSqlServerTableMetadataParser(schema);
    }

    @Test
    public void shouldConnectToRealSqlServerMode() throws Exception {
        Map<String, Object> server = executor.select(SqlRequests.of(
            "select version() as version, current_setting('database_mode') as mode"), ResultWrappers.singleMap());
        Assert.assertEquals("sqlserver", server.get("mode"));
        Assert.assertTrue(String.valueOf(server.get("version")).startsWith("KingbaseES"));
        System.out.println("[SQL Server 实库] " + server);
        System.out.println("[R2DBC 驱动] " + Class.forName("io.r2dbc.kingbase.KingbaseConnectionFactoryProvider")
            .getProtectionDomain().getCodeSource().getLocation());
    }

    @Test
    public void shouldReadCommentsAndDimensionsThroughSyncAndReactiveMetadata() {
        operator.ddl().createOrAlter("catalog_probe").comment("账户 owner's table")
            .addColumn("id").varchar(32).primaryKey().comment("主键 owner's id").commit()
            .addColumn("name").type(JDBCType.NVARCHAR, String.class).length(64).comment("账户名称").commit()
            .addColumn("amount").number(18, 4).commit()
            .addColumn("payload").type(JDBCType.BLOB, byte[].class).commit()
            .commit().sync();
        assertCatalog(parser.parseByName("catalog_probe").orElseThrow(AssertionError::new));
        parser.parseByNameReactive("catalog_probe").as(StepVerifier::create)
            .assertNext(KingbaseSqlServerR2dbcIntegrationTests::assertCatalog).verifyComplete();
        operator.ddl().createOrAlter("catalog_probe")
            .addColumn("name").type(JDBCType.NVARCHAR, String.class).length(128).comment("修改 owner's name").commit()
            .commit().sync();
        RDBColumnMetadata name = parser.parseByName("catalog_probe").orElseThrow(AssertionError::new)
            .getColumn("name").orElseThrow(AssertionError::new);
        Assert.assertEquals(128, name.getLength());
        Assert.assertEquals("修改 owner's name", name.getComment());
    }

    @Test
    public void shouldReadNativeIndexFlagsOrderingAndLookup() {
        executor.execute(SqlRequests.of("create table index_probe (id varchar(32) primary key, name varchar(64), age int)"));
        executor.execute(SqlRequests.of("create unique index idx_probe_name_age on index_probe(name asc, age desc)"));
        KingbaseSqlServerIndexMetadataParser indexes = new KingbaseSqlServerIndexMetadataParser(schema);
        List<RDBIndexMetadata> actual = indexes.parseTableIndex("index_probe");
        Assert.assertEquals(2, actual.size());
        Assert.assertTrue(actual.stream().anyMatch(RDBIndexMetadata::isPrimaryKey));
        assertComposite(indexes.parseByName("idx_probe_name_age").orElseThrow(AssertionError::new));
        indexes.parseByNameReactive("idx_probe_name_age").as(StepVerifier::create)
            .assertNext(KingbaseSqlServerR2dbcIntegrationTests::assertComposite).verifyComplete();
        indexes.parseTableIndexReactive("index_probe").as(StepVerifier::create).expectNextCount(2).verifyComplete();
        Assert.assertFalse(indexes.parseByName("missing_index").isPresent());
    }

    @Test
    public void shouldScopeTableExistencePrimaryKeysAndParseAllToTheSelectedSchema() {
        String other = schema.getName() + "_other";
        executor.execute(SqlRequests.of("create schema [" + other + "]"));
        try {
            executor.execute(SqlRequests.of("create table isolation_probe (id varchar(32),"
                + " name varchar(64), constraint business_identity primary key(id))"));
            executor.execute(SqlRequests.of("create table [" + other + "].isolation_probe(id varchar(16), extra int)"));
            executor.execute(SqlRequests.of("create table [" + other + "].only_in_other(id int)"));
            Assert.assertFalse(parser.tableExists("only_in_other"));
            Assert.assertFalse(parser.parseAllTableName().contains("only_in_other"));
            parser.tableExistsReactive("only_in_other").as(StepVerifier::create).expectNext(false).verifyComplete();
            RDBTableMetadata table = parser.parseByName("isolation_probe").orElseThrow(AssertionError::new);
            Assert.assertTrue(table.getColumn("id").orElseThrow(AssertionError::new).isPrimaryKey());
            Assert.assertEquals(32, table.getColumn("id").orElseThrow(AssertionError::new).getLength());
            Assert.assertFalse(table.getColumn("extra").isPresent());
            Assert.assertTrue(parser.parseAll().stream().anyMatch(t -> "isolation_probe".equals(t.getName())));
            parser.parseAllReactive().collectList().as(StepVerifier::create).assertNext(tables -> {
                Assert.assertTrue(tables.stream().anyMatch(t -> "isolation_probe".equals(t.getName())));
                Assert.assertFalse(tables.stream().anyMatch(t -> "only_in_other".equals(t.getName())));
            }).verifyComplete();
        } finally {
            executor.execute(SqlRequests.of("drop schema [" + other + "] cascade"));
        }
    }

    @Test
    public void shouldTranslateRealDuplicateKeysForBothOrmApis() {
        operator.ddl().createOrAlter("duplicate_probe")
            .addColumn("id").varchar(32).primaryKey().commit().commit().sync();
        operator.dml().insert("duplicate_probe").columns("id").values("duplicate").execute().sync();
        Assert.assertThrows(DuplicateKeyException.class,
            () -> operator.dml().insert("duplicate_probe").columns("id").values("duplicate").execute().sync());
        operator.dml().insert("duplicate_probe").columns("id").values("duplicate").execute().reactive()
            .as(StepVerifier::create).expectError(DuplicateKeyException.class).verify();
    }

    private static void assertCatalog(RDBTableMetadata table) {
        Assert.assertEquals("账户 owner's table", table.getComment());
        RDBColumnMetadata id = table.getColumn("id").orElseThrow(AssertionError::new);
        Assert.assertTrue(id.isPrimaryKey());
        Assert.assertEquals("主键 owner's id", id.getComment());
        RDBColumnMetadata name = table.getColumn("name").orElseThrow(AssertionError::new);
        Assert.assertEquals(JDBCType.NVARCHAR, name.getSqlType());
        Assert.assertEquals(64, name.getLength());
        Assert.assertEquals("nvarchar(64)", name.getDataType());
        Assert.assertEquals("账户名称", name.getComment());
        RDBColumnMetadata amount = table.getColumn("amount").orElseThrow(AssertionError::new);
        Assert.assertEquals(18, amount.getPrecision());
        Assert.assertEquals(4, amount.getScale());
        Assert.assertEquals("numeric(18,4)", amount.getDataType());
        Assert.assertEquals("varbinary(max)", table.getColumn("payload").orElseThrow(AssertionError::new).getDataType());
    }

    private static void assertComposite(RDBIndexMetadata index) {
        Assert.assertEquals("index_probe", index.getTableName());
        Assert.assertTrue(index.isUnique());
        Assert.assertFalse(index.isPrimaryKey());
        Assert.assertEquals(Arrays.asList("name", "age"), Arrays.asList(
            index.getColumns().get(0).getColumn(), index.getColumns().get(1).getColumn()));
        Assert.assertEquals(RDBIndexMetadata.IndexSort.asc, index.getColumns().get(0).getSort());
        Assert.assertEquals(RDBIndexMetadata.IndexSort.desc, index.getColumns().get(1).getSort());
    }
}
