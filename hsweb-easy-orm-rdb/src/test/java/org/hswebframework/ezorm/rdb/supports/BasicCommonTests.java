package org.hswebframework.ezorm.rdb.supports;

import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.dml.query.SortOrder;
import org.junit.Assert;
import org.junit.Test;

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
    public void testPager() {
        RDBDatabaseMetadata database = getDatabase();
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);

        try {
            operator.ddl()
                    .createOrAlter("test_pager")
                    .addColumn().name("id").number(32).primaryKey().comment("ID").commit()
                    .commit();

            for (int i = 0; i < 100; i++) {
                operator.dml()
                        .insert("test_pager")
                        .value("id", i + 1)
                        .execute()
                        .sync();
            }


            for (int i = 0; i < 10; i++) {
                long sum = operator.dml()
                        .query()
                        .select("id")
                        .from("test_pager")
//                        .orderBy(asc("id"))
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
                    .commit();

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
                    .where(dsl -> dsl.where("comment", 2))
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
                    .commit();

            //alter
            operator.ddl()
                    .createOrAlter("test_ddl_create")
                    .addColumn().name("name").varchar(128).comment("名称").commit()
                    .addColumn().name("test").varchar(32).comment("test").commit()
                    .addColumn().name("age").number(4).defaultValue("0").comment("年龄").commit()
                    .commit();

            //drop column
            operator.ddl()
                    .createOrAlter("test_ddl_create")
                    .dropColumn("test")
                    .commit();


        } finally {
            try {
                operator.sql().sync().execute(SqlRequests.of("drop table " + database.getCurrentSchema().getName() + ".test_ddl_create"));
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

    }

}
