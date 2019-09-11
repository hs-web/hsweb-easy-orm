package org.hswebframework.ezorm.rdb.supports;

import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperator;
import org.hswebframework.ezorm.rdb.operator.dml.query.SortOrder;
import org.junit.Assert;
import org.junit.Test;
import reactor.core.publisher.Mono;

import java.util.stream.Collectors;

import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.map;
import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.mapStream;
import static org.hswebframework.ezorm.rdb.operator.dml.query.SortOrder.*;

public abstract class BasicCommonTests {

    protected abstract RDBSchemaMetadata getSchema();

    protected abstract Dialect getDialect();

    protected abstract SyncSqlExecutor getSqlExecutor();

    protected RDBDatabaseMetadata getDatabase() {
        RDBDatabaseMetadata metadata = new RDBDatabaseMetadata(getDialect());

        RDBSchemaMetadata schema = getSchema();

        metadata.setCurrentSchema(schema);
        metadata.addSchema(schema);
        metadata.addFeature(getSqlExecutor());

        return metadata;
    }


    @Test
    public void testReactive() {
        RDBDatabaseMetadata database = getDatabase();
        if (!database.supportFeature(ReactiveSqlExecutor.id)) {
            return;
        }
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);

        try {
            operator.ddl()
                    .createOrAlter("test_reactive_pager")
                    .addColumn().name("id").number(32).primaryKey().comment("ID").commit()
                    .commit()
                    .reactive()
                    .block();

            InsertOperator insert = operator.dml()
                    .insert("test_reactive_pager")
                    .columns("id");

            for (int i = 0; i < 100; i++) {
                insert.values(i + 1);
            }

            insert.execute()
                    .reactive()
                    .block();

            for (int i = 0; i < 10; i++) {
                long sum = operator.dml()
                        .query()
                        .select("id")
                        .from("test_reactive_pager")
                        .paging(i, 10)
                        .fetch(map())
                        .reactive()
                        .map(map -> map.get("id"))
                        .map(Number.class::cast)
                        .collect(Collectors.summingInt(Number::intValue))
                        .blockOptional()
                        .orElse(0);

                Assert.assertEquals(sum, (((i * 10) + 1) + ((i + 1) * 10)) * 10 / 2);
            }
        } finally {
            try {
                operator.sql().reactive()
                        .execute(Mono.just(SqlRequests.of("drop table " + database.getCurrentSchema().getName() + ".test_reactive_pager")))
                        .block();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

    }

    @Test
    public void testPager() {
        RDBDatabaseMetadata database = getDatabase();
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);

        try {
            operator.ddl()
                    .createOrAlter("test_pager")
                    .addColumn().name("id").number(32).primaryKey().comment("ID").commit()
                    .commit()
                    .sync();

            InsertOperator insert = operator.dml()
                    .insert("test_pager")
                    .columns("id");

            for (int i = 0; i < 100; i++) {
                insert.values(i + 1);
            }
            insert.execute().sync();

            for (int i = 0; i < 10; i++) {
                long sum = operator.dml()
                        .query()
                        .select("id")
                        .from("test_pager")
//                        .orderBy(desc("id"))
                        .paging(i, 10)
                        .fetch(mapStream())
                        .sync()
                        .map(map -> map.get("id"))
                        .map(Number.class::cast)
                        .mapToInt(Number::intValue)
                        .sum();

                Assert.assertEquals(sum, (((i * 10) + 1) + ((i + 1) * 10)) * 10 / 2);
            }
        } finally {
            try {
                operator.sql().sync().execute(SqlRequests.of("drop table " + database.getCurrentSchema().getName() + ".test_pager"));
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    @Test
    public void testCrud() {
        RDBDatabaseMetadata database = getDatabase();
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);
        try {
            operator.ddl()
                    .createOrAlter("test_dml_crud")
                    .alias("dml")
                    .comment("dmlTest")
                    .addColumn().name("id").varchar(32).primaryKey().comment("ID").commit()
                    .addColumn().name("name").varchar(64).notNull().comment("名称").commit()
                    .addColumn().name("comment").varchar(32).defaultValue("'1'").commit()
                    .commit().sync();

            operator.dml()
                    .insert("test_dml_crud")
                    .value("id", "1234")
                    .value("name", "名称")
                    .execute()
                    .sync();

            int updated = operator.dml()
                    .update("test_dml_crud")
                    .set("comment", "2")
                    .where(dsl -> dsl.where("id", "1234"))
                    .execute()
                    .sync();

            Assert.assertEquals(updated, 1);

            int sum = operator.dml()
                    .query()
                    .select("comment")
                    .from("test_dml_crud")
                    .fetch(mapStream())
                    .sync()
                    .map(map -> map.get("comment"))
                    .map(String::valueOf)
                    .mapToInt(Integer::valueOf)
                    .sum();
            Assert.assertEquals(sum, 2);

            int deleted = operator.dml().delete("test_dml_crud")
                    .where(dsl -> dsl.where("comment", "2"))
                    .execute()
                    .sync();
            Assert.assertEquals(deleted, 1);

        } finally {
            try {
                operator.sql().sync().execute(SqlRequests.of("drop table " + database.getCurrentSchema().getName() + ".test_dml_crud"));
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    @Test
    public void testDDL() {

        RDBDatabaseMetadata database = getDatabase();
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);

        try {
            operator.ddl()
                    .createOrAlter("test_ddl_create")
                    .alias("test")
                    .comment("测试")
                    .addColumn().name("id").varchar(32).primaryKey().comment("ID").commit()
                    .addColumn().name("name").varchar(64).notNull().comment("名称").commit()
                    .addColumn().name("comment").varchar(32).defaultValue("'1'").commit()
                    .index().name("index_").column("name").commit()
                    .commit().sync();

            //alter
            operator.ddl()
                    .createOrAlter("test_ddl_create")
                    .addColumn().name("name").varchar(128).comment("名称").commit()
                    .addColumn().name("test").varchar(32).comment("test").commit()
                    .addColumn().name("age").number(4).defaultValue("0").comment("年龄").commit()
                    .commit().sync();

            //drop column
            operator.ddl()
                    .createOrAlter("test_ddl_create")
                    .dropColumn("test")
                    .commit().sync();


        } finally {
            try {
                operator.sql().sync().execute(SqlRequests.of("drop table " + database.getCurrentSchema().getName() + ".test_ddl_create"));
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

    }

}
