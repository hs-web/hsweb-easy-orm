package org.hswebframework.ezorm.rdb.operator.dml.upsert;

import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.mapping.defaults.SaveResult;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.supports.h2.H2ConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.h2.H2R2dbcConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import reactor.test.StepVerifier;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.UUID;

import static org.junit.Assert.*;

public class DefaultSaveOrUpdateOperatorTest {


    private DatabaseOperator databaseOperator;

    @Before
    public void init() {

        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.H2);
        RDBSchemaMetadata schema = new H2SchemaMetadata("PUBLIC");

        database.addFeature(new TestSyncSqlExecutor(new H2ConnectionProvider()));
        database.addFeature(new TestReactiveSqlExecutor(new H2R2dbcConnectionProvider()));

        database.addSchema(schema);
        database.setCurrentSchema(schema);
        databaseOperator = DefaultDatabaseOperator.of(database);

    }


    @Test
    public void testSync() {
        databaseOperator.ddl()
                .createOrAlter("upsert_test")
                .addColumn("id").defaultValueRuntime(() -> UUID.randomUUID().toString().replace("-", "")).primaryKey().varchar(32).commit()
                .addColumn("name").varchar(32).commit()
                .commit()
                .sync();

        SaveResult result = databaseOperator
                .dml()
                .upsert("upsert_test")
                .value("id", "123")
                .value("name", "test")
                .execute()
                .sync();
        Assert.assertEquals(result.getAdded(), 1);


        result = databaseOperator
                .dml()
                .upsert("upsert_test")
                .value("id", "123")
                .value("name", "test")
                .execute()
                .sync();
        Assert.assertEquals(result.getUpdated(), 1);

        result = databaseOperator
                .dml()
                .upsert("upsert_test")
                .columns("id", "name")
                .values("123", "test")
                .values("234", "test2")
                .values("2345", "test3")
                .execute()
                .sync();
        Assert.assertEquals(result.getUpdated(), 1);
        Assert.assertEquals(result.getAdded(), 2);

        result = databaseOperator
                .dml()
                .upsert("upsert_test")
                .columns("name")
                .values("test")
                .values("test2")
                .values("test3")
                .execute()
                .sync();
        Assert.assertEquals(result.getAdded(), 3);

    }

    @Test
    public void testReactive() {
        databaseOperator.ddl()
                .createOrAlter("upsert_test")
                .addColumn("id").defaultValueRuntime(() -> UUID.randomUUID().toString().replace("-", "")).primaryKey().varchar(32).commit()
                .addColumn("name").varchar(32).commit()
                .commit()
                .reactive()
                .as(StepVerifier::create)
                .expectNext(true)
                .verifyComplete();

        databaseOperator
                .dml()
                .upsert("upsert_test")
                .value("id", "123")
                .value("name", "test")
                .execute()
                .reactive()
                .map(SaveResult::getAdded)
                .as(StepVerifier::create)
                .expectNext(1)
                .verifyComplete();


        databaseOperator
                .dml()
                .upsert("upsert_test")
                .value("id", "123")
                .value("name", "test")
                .execute()
                .reactive()
                .map(SaveResult::getUpdated)
                .as(StepVerifier::create)
                .expectNext(1)
                .verifyComplete();

         databaseOperator
                .dml()
                .upsert("upsert_test")
                .columns("id", "name")
                .values("123", "test")
                .values("234", "test2")
                .values("2345", "test3")
                .execute()
                .reactive()
                .map(SaveResult::getTotal)
                .as(StepVerifier::create)
                .expectNext(3)
                .verifyComplete();

        databaseOperator
                .dml()
                .upsert("upsert_test")
                .columns("name")
                .values("test")
                .values("test2")
                .values("test3")
                .execute()
                .reactive()
                .map(SaveResult::getAdded)
                .as(StepVerifier::create)
                .expectNext(3)
                .verifyComplete();


    }

    @Test
    public void testSyncBatchValuesIgnoreUpdateBeforeValues() {
        databaseOperator.ddl()
                .createOrAlter("upsert_ignore_test")
                .addColumn("id").primaryKey().varchar(32).commit()
                .addColumn("name").varchar(32).commit()
                .addColumn("age").integer().commit()
                .commit()
                .sync();

        SaveResult result = databaseOperator
                .dml()
                .upsert("upsert_ignore_test")
                .value("id", "123")
                .value("name", "old")
                .value("age", 1)
                .execute()
                .sync();

        Assert.assertEquals(1, result.getAdded());

        result = databaseOperator
                .dml()
                .upsert("upsert_ignore_test")
                .ignoreUpdate("name")
                .values(Arrays.asList(
                        new LinkedHashMap<String, Object>() {{
                            put("id", "123");
                            put("name", "new");
                            put("age", 2);
                        }},
                        new LinkedHashMap<String, Object>() {{
                            put("id", "234");
                            put("name", "created");
                            put("age", 3);
                        }}
                ))
                .execute()
                .sync();

        Assert.assertEquals(1, result.getUpdated());
        Assert.assertEquals(1, result.getAdded());

        Map<String, Object> updated = databaseOperator
                .dml()
                .query("upsert_ignore_test")
                .select("id", "name", "age")
                .where(dsl -> dsl.is("id", "123"))
                .fetch(ResultWrappers.lowerCase(ResultWrappers.singleMap()))
                .sync();

        Assert.assertEquals("123", updated.get("id"));
        Assert.assertEquals("old", updated.get("name"));
        Assert.assertEquals(2, ((Number) updated.get("age")).intValue());

        Map<String, Object> created = databaseOperator
                .dml()
                .query("upsert_ignore_test")
                .select("id", "name", "age")
                .where(dsl -> dsl.is("id", "234"))
                .fetch(ResultWrappers.lowerCase(ResultWrappers.singleMap()))
                .sync();

        Assert.assertEquals("234", created.get("id"));
        Assert.assertEquals("created", created.get("name"));
        Assert.assertEquals(3, ((Number) created.get("age")).intValue());
    }

    @Test
    public void testSyncIgnoreUpdateColumnMissingInAllValues() {
        databaseOperator.ddl()
                .createOrAlter("upsert_ignore_missing_all_test")
                .addColumn("id").primaryKey().varchar(32).commit()
                .addColumn("name").varchar(32).commit()
                .addColumn("age").integer().commit()
                .commit()
                .sync();

        SaveResult result = databaseOperator
                .dml()
                .upsert("upsert_ignore_missing_all_test")
                .ignoreUpdate("name")
                .values(Arrays.asList(
                        new LinkedHashMap<String, Object>() {{
                            put("id", "100");
                            put("age", 10);
                        }},
                        new LinkedHashMap<String, Object>() {{
                            put("id", "200");
                            put("age", 20);
                        }}
                ))
                .execute()
                .sync();

        Assert.assertEquals(2, result.getAdded());

        Map<String, Object> data = databaseOperator
                .dml()
                .query("upsert_ignore_missing_all_test")
                .select("id", "name", "age")
                .where(dsl -> dsl.is("id", "100"))
                .fetch(ResultWrappers.lowerCase(ResultWrappers.singleMap()))
                .sync();

        Assert.assertEquals("100", data.get("id"));
        Assert.assertNull(data.get("name"));
        Assert.assertEquals(10, ((Number) data.get("age")).intValue());
    }

    @Test
    public void testSyncIgnoreUpdateColumnMissingInPartialValues() {
        databaseOperator.ddl()
                .createOrAlter("upsert_ignore_missing_partial_test")
                .addColumn("id").primaryKey().varchar(32).commit()
                .addColumn("name").varchar(32).commit()
                .addColumn("age").integer().commit()
                .commit()
                .sync();

        SaveResult result = databaseOperator
                .dml()
                .upsert("upsert_ignore_missing_partial_test")
                .value("id", "100")
                .value("name", "old")
                .value("age", 1)
                .execute()
                .sync();

        Assert.assertEquals(1, result.getAdded());

        result = databaseOperator
                .dml()
                .upsert("upsert_ignore_missing_partial_test")
                .ignoreUpdate("name")
                .values(Arrays.asList(
                        new LinkedHashMap<String, Object>() {{
                            put("id", "100");
                            put("age", 11);
                        }},
                        new LinkedHashMap<String, Object>() {{
                            put("id", "200");
                            put("name", "created");
                            put("age", 22);
                        }}
                ))
                .execute()
                .sync();

        Assert.assertEquals(1, result.getUpdated());
        Assert.assertEquals(1, result.getAdded());

        Map<String, Object> updated = databaseOperator
                .dml()
                .query("upsert_ignore_missing_partial_test")
                .select("id", "name", "age")
                .where(dsl -> dsl.is("id", "100"))
                .fetch(ResultWrappers.lowerCase(ResultWrappers.singleMap()))
                .sync();

        Assert.assertEquals("100", updated.get("id"));
        Assert.assertEquals("old", updated.get("name"));
        Assert.assertEquals(11, ((Number) updated.get("age")).intValue());

        Map<String, Object> created = databaseOperator
                .dml()
                .query("upsert_ignore_missing_partial_test")
                .select("id", "name", "age")
                .where(dsl -> dsl.is("id", "200"))
                .fetch(ResultWrappers.lowerCase(ResultWrappers.singleMap()))
                .sync();

        Assert.assertEquals("200", created.get("id"));
        Assert.assertEquals("created", created.get("name"));
        Assert.assertEquals(22, ((Number) created.get("age")).intValue());
    }

}
