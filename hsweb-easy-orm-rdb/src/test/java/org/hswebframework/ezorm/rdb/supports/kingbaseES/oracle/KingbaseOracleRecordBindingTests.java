package org.hswebframework.ezorm.rdb.supports.kingbaseES.oracle;

import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.mapping.ReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.SyncRepository;
import org.hswebframework.ezorm.rdb.mapping.defaults.record.Record;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.supports.kingbaseES.KingbaseOfficialTestDatabase;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import java.time.Duration;
import java.util.Arrays;
import java.util.Map;

/** Record CRUD using keys that exactly match the parsed column properties. */
public class KingbaseOracleRecordBindingTests {

    @ClassRule
    public static final KingbaseOfficialTestDatabase DATABASE = new KingbaseOfficialTestDatabase("oracle");

    private static final Duration TIMEOUT = Duration.ofSeconds(30);
    private static DatabaseOperator operator;
    private static RDBTableMetadata table;

    @BeforeClass
    public static void createTableAndReadMetadata() {
        RDBSchemaMetadata schema = DATABASE.newSchema();
        DATABASE.sync().execute(SqlRequests.of("create table record_case_test ("
            + "\"id\" varchar2(32) primary key, name varchar2(128) not null, age number(10),"
            + "\"MixedCase\" varchar2(32))"));
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(schema.getDialect());
        database.addSchema(schema);
        database.setCurrentSchema(schema);
        database.addFeature(DATABASE.sync());
        database.addFeature(DATABASE.reactive());
        table = schema.getTableReactive("record_case_test", true).block(TIMEOUT);
        Assert.assertNotNull(table);
        schema.addTable(table);
        operator = DefaultDatabaseOperator.of(database);
    }

    @Test
    public void shouldSaveMatchingRecordKeysThroughSyncRepository() {
        SyncRepository<Record, String> repository = operator.dml().createRepository("record_case_test");
        Assert.assertEquals(1, repository.save(Record.newRecord().putValue("id", "sync")
            .putValue("name", "saved").putValue("age", 7).putValue("MixedCase", "quoted")).getTotal());
        assertRow("sync", "saved", 7, "quoted");
        Assert.assertEquals(1, repository.createUpdate().where("id", "sync").set("name", "updated").execute());
        assertRow("sync", "updated", 7, "quoted");
        Assert.assertTrue(repository.findById("sync").isPresent());
        Assert.assertEquals(1, repository.createDelete().where("id", "sync").execute());
        Assert.assertFalse(repository.findById("sync").isPresent());
        Assert.assertEquals("id", table.getColumnNow("id").getRealName());
        Assert.assertEquals("name", table.getColumnNow("name").getRealName());
        Assert.assertEquals("MixedCase", table.getColumnNow("MixedCase").getRealName());
    }

    @Test
    public void shouldInsertAndBatchSaveMatchingKeysThroughReactiveRepository() {
        ReactiveRepository<Record, String> repository = operator.dml().createReactiveRepository("record_case_test");
        Assert.assertEquals(Integer.valueOf(1), repository.insert(Record.newRecord()
            .putValue("id", "reactive-insert").putValue("name", "inserted")
            .putValue("age", 8).putValue("MixedCase", "quoted-insert")).block(TIMEOUT));
        assertRow("reactive-insert", "inserted", 8, "quoted-insert");
        Assert.assertEquals(2, repository.save(Arrays.asList(
            Record.newRecord().putValue("id", "reactive-insert").putValue("name", "changed")
                .putValue("age", 9).putValue("MixedCase", "quoted-changed"),
            Record.newRecord().putValue("id", "reactive-new").putValue("name", "exact")
                .putValue("age", 10))).block(TIMEOUT).getTotal());
        assertRow("reactive-insert", "changed", 9, "quoted-changed");
        assertRow("reactive-new", "exact", 10, null);
    }

    private static void assertRow(String id, String name, int age, String mixedCase) {
        Map<String, Object> row = DATABASE.reactive().select(SqlRequests.of(
            "select \"id\", \"name\", \"age\", \"MixedCase\" from " + table.getFullName()
                + " where \"id\" = ?", id), ResultWrappers.lowerCase(ResultWrappers.map()))
            .single().block(TIMEOUT);
        Assert.assertNotNull(row);
        Assert.assertEquals(id, row.get("id"));
        Assert.assertEquals(name, row.get("name"));
        Assert.assertEquals(age, ((Number) row.get("age")).intValue());
        Assert.assertEquals(mixedCase, row.get("mixedcase"));
    }
}
