package org.hswebframework.ezorm.rdb.supports.oracle;

import org.hswebframework.ezorm.rdb.TestJdbcReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.SyncRepository;
import org.hswebframework.ezorm.rdb.mapping.defaults.record.Record;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatureType;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.builder.Paginator;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.math.BigDecimal;
import java.sql.JDBCType;

import static org.hswebframework.ezorm.rdb.executor.SqlRequests.prepare;

public class OracleTableMetaParserTest {

    OracleSchemaMetadata schema;

    private SyncSqlExecutor executor;

    private OracleTableMetadataParser parser;

    @Before
    public void init() {
        executor = new TestSyncSqlExecutor(new OracleConnectionProvider());
        schema = new OracleSchemaMetadata("SYSTEM");
        schema.addFeature(executor);
        schema.addFeature(new TestJdbcReactiveSqlExecutor(new OracleConnectionProvider()));

        parser = new OracleTableMetadataParser(schema);
    }

    @Test
    public void testParseReactive() {
//        schema.loadAllTableReactive()
//              .as(StepVerifier::create)
//              .expectComplete()
//              .verify();


        parser.parseAllTableNameReactive()
              .doOnNext(System.out::println)
              .blockLast();

//        parser.parseAllReactive()
//              .doOnNext(table -> System.out.println(table.getName()))
//              .then()
//              .as(StepVerifier::create)
//              .expectComplete()
//              .verify();
    }

    @Test
    public void testParseAll() {
        executor.execute(SqlRequests.of("CREATE TABLE \"test_table_all\"(" +
                                            "\"id\" varchar2(32) primary key," +
                                            "name varchar2(128) not null," +
                                            "age number(10)" +
                                            ")"));
        try {
            for (RDBTableMetadata rdbTableMetadata : parser.parseAll()) {
                System.out.println(rdbTableMetadata.getName() + " => " + rdbTableMetadata.getRealName());
                schema.addTable(rdbTableMetadata);
            }

            RDBTableMetadata metadata = schema
                .getTable("test_table_all", false)
                .orElseThrow(NullPointerException::new);

            Assert.assertEquals("test_table_all", metadata.getRealName());

            for (RDBColumnMetadata column : metadata.getColumns()) {
                System.out.println(column);
            }

            RDBDatabaseMetadata
                databaseMetadata = new RDBDatabaseMetadata(Dialect.ORACLE);
            databaseMetadata.addSchema(schema);
            databaseMetadata.setCurrentSchema(schema);
            databaseMetadata.addFeature(executor);
            DatabaseOperator databaseOperator = DefaultDatabaseOperator.of(databaseMetadata);

            SyncRepository<Record, String> repository = databaseOperator
                .dml()
                .createRepository("test_table_all");

            repository
                .save(Record.newRecord().putValue("id", "test").putValue("NAME", "test").putValue("AGE", 1));

            repository.findById("test");

            repository.createUpdate()
                      .where("id", "test")
                      .set("NAME", "test2")
                      .execute();

            System.out.println(repository
                                   .createQuery()
                                   .paging(0, 10)
                                   .fetch());

            repository
                .createDelete()
                .where("id", "test")
                .execute();
        } finally {
            executor.execute(prepare("drop table \"test_table_all\""));
        }
    }

    @Test
    public void testParse() {

        try {
            executor.execute(SqlRequests.of("CREATE TABLE test_table(" +
                                                "id varchar2(32) primary key," +
                                                "name varchar2(128) not null," +
                                                "age number(10)" +
                                                ")"));
            RDBTableMetadata metaData = parser.parseByName("test_table").orElseThrow(NullPointerException::new);

            //id
            {
                RDBColumnMetadata column = metaData.getColumn("id").orElseThrow(NullPointerException::new);

                Assert.assertNotNull(column);

                Assert.assertEquals("varchar2(32 char)", column.getDataType());
                Assert.assertEquals(JDBCType.VARCHAR, column.getType().getSqlType());
                Assert.assertEquals(String.class, column.getJavaType());
                Assert.assertTrue(column.isNotNull());
                // 这里只解析表结构，而不会解析键信息.
                // Assert.assertTrue(column.isPrimaryKey());
            }

            //name
            {
                RDBColumnMetadata column = metaData.getColumn("name").orElseThrow(NullPointerException::new);

                Assert.assertNotNull(column);

                Assert.assertEquals(column.getDataType(), "varchar2(128 char)");
                Assert.assertEquals(column.getLength(), 128);
                Assert.assertEquals(column.getType().getSqlType(), JDBCType.VARCHAR);
                Assert.assertEquals(column.getJavaType(), String.class);
                Assert.assertTrue(column.isNotNull());
            }

            //age
            {
                RDBColumnMetadata column = metaData.getColumn("age").orElseThrow(NullPointerException::new);

                Assert.assertNotNull(column);
                Assert.assertEquals(column.getPrecision(), 10);
                Assert.assertEquals(column.getScale(), 0);
                Assert.assertEquals(column.getDataType(), "number(10,0)");
                Assert.assertEquals(column.getType().getSqlType(), JDBCType.NUMERIC);
                Assert.assertEquals(column.getJavaType(), BigDecimal.class);
            }
        } finally {
            executor.execute(prepare("drop table test_table"));
        }

    }


}