package org.hswebframework.ezorm.rdb.supports.mysql;

import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.junit.Before;
import org.junit.Test;
import reactor.test.StepVerifier;

/**
 * @author bestfeng
 * @since 1.9
 */
public class MysqlSaveOrUpdateOperatorTest {

    DatabaseOperator databaseOperator;

    @Before
    public void init() {

        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.MYSQL);
        RDBSchemaMetadata schema = new MysqlSchemaMetadata("ezorm");

        database.addFeature(new TestSyncSqlExecutor(new Mysql57ConnectionProvider()));
        database.addFeature(new TestReactiveSqlExecutor(new MysqlR2dbcConnectionProvider()));

        database.addSchema(schema);
        database.setCurrentSchema(schema);
        database.setName("ezorm");
        databaseOperator = DefaultDatabaseOperator.of(database);

        databaseOperator
                .sql()
                .sync()
                .execute(SqlRequests.of("CREATE TABLE IF NOT EXISTS ezorm.test(" +
                                                "id varchar(32) primary key," +
                                                "name varchar(128) not null," +
                                                "age int" +
                                                ")"));


    }

    @Test
    public void testSaveOrUpdateNotColumn() {
        databaseOperator
                .dml()
                .upsert("test")
                .value("id", "123")
                .value("name", "aaa")
                 .ignoreUpdate("name")
                .execute()
                .sync();
    }
}
