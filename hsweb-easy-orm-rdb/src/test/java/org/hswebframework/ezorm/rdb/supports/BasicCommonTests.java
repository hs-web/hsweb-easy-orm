package org.hswebframework.ezorm.rdb.supports;

import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.junit.Assert;
import org.junit.Test;

import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.mapStream;

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
    public void testDDL() {

        RDBDatabaseMetadata database = getDatabase();
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);

        try {
            operator.ddl()
                    .createOrAlter("test_ddl_create")
                    .addColumn().name("id").varchar(32).primaryKey().comment("ID").commit()
                    .addColumn().name("name").varchar(64).notNull().comment("名称").commit()
                    .addColumn().name("comment").varchar(32).defaultValue("'1'").commit()
                    .index().name("index_").column("name").commit()
                    .commit();

            operator.ddl()
                    .createOrAlter("test_ddl_create")
                    .addColumn().name("name").varchar(128).comment("名称").commit()
                    .addColumn().name("age").number(4).defaultValue("0").comment("年龄").commit()
                    .commit();

            operator.ddl()
                    .createOrAlter("test_ddl_create")
                   // .dropColumn("age")
                    .commit();

            operator.dml()
                    .insert("test_ddl_create")
                    .value("id", "1234")
                    .value("name", "名称")
                    .execute()
                    .sync();

            int sum = operator.dml()
                    .query()
                    .select("comment")
                    .from("test_ddl_create")
                    .fetch(mapStream())
                    .sync()
                    .map(map -> map.get("comment"))
                    .map(String::valueOf)
                    .mapToInt(Integer::valueOf)
                    .sum();
            Assert.assertEquals(sum, 1);
        } finally {
            try {
                operator.sql().sync().execute(SqlRequests.of("drop table " + database.getCurrentSchema().getName() + ".test_ddl_create"));
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

    }

}
