package org.hswebframework.ezorm.rdb.operator;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hswebframework.ezorm.rdb.TestJdbcReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.h2.H2ConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.h2.H2TableMetadataParser;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.io.Serializable;
import java.util.Map;

import static org.hswebframework.ezorm.rdb.executor.SqlRequests.of;
import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.*;

public class DefaultDatabaseOperatorTest {

    private RDBDatabaseMetadata database;

    private DatabaseOperator operator;

    @Before
    public void init() {
        database = new RDBDatabaseMetadata(Dialect.H2);

        SyncSqlExecutor sqlExecutor = new TestSyncSqlExecutor(new H2ConnectionProvider());
        database.addFeature(sqlExecutor);
        database.addFeature(new TestJdbcReactiveSqlExecutor(new H2ConnectionProvider()));
        H2SchemaMetadata schema = new H2SchemaMetadata("PUBLIC");



        schema.addFeature(new H2TableMetadataParser(schema));
        database.addSchema(schema);
        database.setCurrentSchema(schema);


        operator = DefaultDatabaseOperator.of(database);

    }


    @Test
    public void testDDLCreate() {
        operator.ddl()
                .createOrAlter("test_ddl_create")
                .addColumn().name("id").varchar(32).primaryKey().comment("ID").commit()
                .addColumn().name("name").varchar(64).notNull().comment("名称").commit()
                .addColumn().name("comment").columnDef("varchar(32) not null default '1'").commit()
                .index().name("index_").column("name").commit()
                .commit()
                .sync();

        operator.dml()
                .insert("test_ddl_create")
                .value("id", "1234")
                .value("name", "名称")
                .execute()
                .sync();

        int sum = operator.dml()
                .query("test_ddl_create")
                .select("comment")
                .fetch(mapStream())
                .sync()
                .map(map -> map.get("comment"))
                .map(String::valueOf)
                .mapToInt(Integer::valueOf)
                .sum();
        Assert.assertEquals(sum, 1);
    }

    @Test
    public void testDropColumn(){
        operator.ddl()
                .createOrAlter("test_ddl_drop")
                .addColumn().name("id").varchar(32).primaryKey().comment("ID").commit()
                .addColumn().name("name").varchar(64).notNull().comment("名称").commit()
                .addColumn().name("comment").columnDef("varchar(32) not null default '1'").commit()
                .commit()
                .sync();

        operator.ddl()
                .createOrAlter("test_ddl_drop")
                .dropColumn("comment")
                .commit()
                .sync();

       Assert.assertFalse(
               operator.getMetadata()
                       .getTable("test_ddl_drop")
                       .flatMap(table->table.getColumn("comment"))
                       .isPresent()
       );
    }

    @Test
    public void testDropColumnReactive(){
        operator.ddl()
                .createOrAlter("test_ddl_drop")
                .addColumn().name("id").varchar(32).primaryKey().comment("ID").commit()
                .addColumn().name("name").varchar(64).notNull().comment("名称").commit()
                .addColumn().name("comment").columnDef("varchar(32) not null default '1'").commit()
                .commit()
                .reactive()
                .block();

        operator.ddl()
                .createOrAlter("test_ddl_drop")
                .dropColumn("comment")
                .commit()
                .reactive()
                .block();

        Assert.assertFalse(
                operator.getMetadata()
                        .getTable("test_ddl_drop")
                        .flatMap(table->table.getColumn("comment"))
                        .isPresent()
        );
    }

    @Getter
    @Setter
    @AllArgsConstructor(staticName = "of")
    @NoArgsConstructor
    public static class TestEntity implements Serializable {
        private String id;
        private String name;

        private int status;
    }

    @Test
    public void testDmlCrud() {

        operator.sql()
                .sync()
                .execute(of("create table test_dm_crud(id varchar(32) primary key,name varchar(64) not null,status number(4) )"));

        TestEntity entity = TestEntity.of("test", "test_name", 1);

        TestEntity entity2 = TestEntity.of("test2", "test2_name", 1);

        int len = operator
                .dml()
                .insert("test_dm_crud")
                .values(entity::getId, entity::getName, entity::getStatus)
                .values(entity2::getId, entity2::getName, entity2::getStatus)
                .execute()
                .sync();
        Assert.assertEquals(len, 2);

        len = operator
                .dml()
                .update("test_dm_crud")
                .set("name", "new_name")
                .where(dsl -> dsl.is("id", "test"))
                .execute()
                .sync();

        Assert.assertEquals(len, 1);


        Map<String, Object> data = operator.dml()
                .query("test_dm_crud")
                .select("id", "name")
                .where(dsl -> dsl.is("id", "test"))
                .fetch(lowerCase(ResultWrappers.singleMap()))
                .sync();
        Assert.assertEquals(data.get("id"), "test");
        Assert.assertEquals(data.get("name"), "new_name");

        len = operator.dml()
                .delete("test_dm_crud")
                .where(dsl -> dsl.is("id", "test"))
                .execute()
                .sync();

        Assert.assertEquals(len, 1);

    }
}