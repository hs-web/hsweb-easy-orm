package org.hswebframework.ezorm.rdb.issues;

import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.supports.h2.H2ConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.h2.H2R2dbcConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.junit.Test;
import reactor.test.StepVerifier;

/**
 * https://github.com/hs-web/hsweb-easy-orm/issues/25
 */
public class Issues25Test {

    @Test
    public void test() {
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.H2);
        H2SchemaMetadata schema = new H2SchemaMetadata("PUBLIC");
        TestReactiveSqlExecutor sqlExecutor = new TestReactiveSqlExecutor(new H2R2dbcConnectionProvider());

        database.addFeature(ReactiveSyncSqlExecutor.of(sqlExecutor));
        database.addFeature(sqlExecutor);

        database.addSchema(schema);
        database.setCurrentSchema(schema);

        DatabaseOperator operator = DefaultDatabaseOperator.of(database);

        operator.ddl()
                .createOrAlter("s_test")
                .addColumn("id").varchar(32).primaryKey().commit()
                .addColumn("name").varchar(32).commit()
                .commit()
                .reactive()
                .as(StepVerifier::create)
                .expectNext(true)
                .verifyComplete();

        operator.dml()
                .query("s_test")
                .where(cdt -> cdt.is("id", "123"))
                .fetch(ResultWrappers.map())
                .reactive()
                .as(StepVerifier::create)
                .expectComplete()
                .verify();

        operator.ddl()
                .createOrAlter("s_test")
                .dropColumn("name")
                .commit()
                .reactive()
                .as(StepVerifier::create)
                .expectNext(true)
                .verifyComplete();

        operator.dml()
                .query("s_test")
                .select("id", "name")
                .where(cdt -> cdt.is("id", "123"))
                .fetch(ResultWrappers.map())
                .reactive()
                .as(StepVerifier::create)
                .expectComplete()
                .verify();
    }

}
