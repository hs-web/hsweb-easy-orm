package org.hswebframework.ezorm.rdb.mapping.defaults.record;

import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.SyncRepository;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.supports.h2.H2ConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.Date;

public class RecordSyncRepositoryTest {

    private SyncRepository<Record, String> repository;

    @Before
    public void init() {
        RDBDatabaseMetadata databaseMetadata = new RDBDatabaseMetadata(Dialect.H2);

        H2SchemaMetadata h2 = new H2SchemaMetadata("PUBLIC");
        databaseMetadata.setCurrentSchema(h2);
        databaseMetadata.addSchema(h2);
        databaseMetadata.addFeature(new TestSyncSqlExecutor(new H2ConnectionProvider()));

        DefaultDatabaseOperator operator = DefaultDatabaseOperator.of(databaseMetadata);
        operator.ddl()
                .createOrAlter("record_test")
                .addColumn("id").primaryKey().varchar(32).commit()
                .addColumn("name").varchar(32).commit()
                .addColumn("state").number(4).commit()
                .addColumn("create_time").alias("createTime").datetime().commit()
                .commit().sync();

        repository =operator.dml().createRepository("record_test");
    }

    @Test
    public void testCrud() {
        repository.newInstance()
                .putValue("id", "test")
                .putValue("name", "test")
                .putValue("state", 1)
                .putValue("create_time", new Date())
                .accept(repository::insert);

        Record record = repository.findById("test").orElseThrow(NullPointerException::new);
        Assert.assertEquals(record.get((Object) "name"), "test");

        Assert.assertEquals(repository.createUpdate()
                .set(repository.newInstance().putValue("state", 2))
                .where("id", "test")
                .execute(), 1);

        Assert.assertEquals(repository.deleteById("test"), 1);
    }

}