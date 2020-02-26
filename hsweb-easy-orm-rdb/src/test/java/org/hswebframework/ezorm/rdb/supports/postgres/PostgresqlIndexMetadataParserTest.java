package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.junit.Assert;
import org.junit.Test;

import java.util.List;

public class PostgresqlIndexMetadataParserTest {

    @Test
    public void test() {

        SyncSqlExecutor sqlExecutor = new TestSyncSqlExecutor(new PostgresqlConnectionProvider());
        try {

            PostgresqlSchemaMetadata schema= new PostgresqlSchemaMetadata("public");

            PostgresqlIndexMetadataParser parser =new PostgresqlIndexMetadataParser(schema);

            parser.getSchema().addFeature(sqlExecutor);

            sqlExecutor.execute(SqlRequests.of(
                    "create table test_index_parser(" +
                            "id varchar(32) primary key," +
                            "name varchar(32) not null," +
                            "age int," +
                            "addr varchar(128))"));



            sqlExecutor.execute(SqlRequests.of("create index test_index on test_index_parser (age)"));

            sqlExecutor.execute(SqlRequests.of("create unique index test_index_2 on test_index_parser (name)"));

            sqlExecutor.execute(SqlRequests.of("create index test_index_3 on test_index_parser (name,addr desc)"));

            List<RDBIndexMetadata> list = parser.parseTableIndex("test_index_parser");

            Assert.assertEquals(list.size(),4);

        } finally {
            try {
                sqlExecutor.execute(SqlRequests.of("drop table test_index_parser"));
            } catch (Exception e) {

            }
        }

    }
}