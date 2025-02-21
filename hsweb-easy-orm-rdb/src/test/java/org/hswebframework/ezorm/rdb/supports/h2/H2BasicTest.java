package org.hswebframework.ezorm.rdb.supports.h2;

import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.exception.DuplicateKeyException;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.defaults.record.Record;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.BasicCommonTests;
import org.hswebframework.ezorm.rdb.supports.BasicTestEntity;
import org.junit.Ignore;
import org.junit.Test;
import reactor.core.publisher.Flux;
import reactor.test.StepVerifier;

public class H2BasicTest extends BasicCommonTests {

    @Override
    protected RDBSchemaMetadata getSchema() {
        return new H2SchemaMetadata("PUBLIC");
    }

    @Override
    protected Dialect getDialect() {
        return Dialect.H2;
    }

    @Override
    protected SyncSqlExecutor getSqlExecutor() {
        return new TestSyncSqlExecutor(new H2ConnectionProvider());
    }

    @Test
    public void test() {
        repository.createQuery()
                  .select("id")
                  .fetch();
    }

    @Ignore
    @Test
    public void testPerform() {
//        for (int i = 0; i < 100; i++) {
//            addressRepository.insert(
//                Record
//                    .newRecord()
//                    .putValue("id", "joinTest_" + i)
//                    .putValue("name", "testPerform"));
//        }
//
        for (int i = 0; i < 1000; i++) {
            addressRepository
                .createQuery()
                .where("name", "testPerform")
                .fetch();
        }


        long time = System.currentTimeMillis();
        for (int i = 0; i < 10_000; i++) {
            addressRepository
                .createQuery()
                .where("name", "testPerform")
                .fetch();
        }

        System.out.println(System.currentTimeMillis() - time);
    }

    @Test
    public void errorTest() {
        BasicTestEntity e = new BasicTestEntity();
        e.setId("id-123");
        e.setName("test");
        e.setState((byte) 1);
        repository.insert(e);
        try {
            repository.insert(e);
            throw new IllegalStateException();
        } catch (DuplicateKeyException ignore) {

        }
    }
}
