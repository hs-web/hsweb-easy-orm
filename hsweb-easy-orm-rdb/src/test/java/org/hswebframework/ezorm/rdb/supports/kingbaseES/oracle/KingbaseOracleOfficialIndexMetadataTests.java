package org.hswebframework.ezorm.rdb.supports.kingbaseES.oracle;

import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.junit.Assert;
import org.junit.Test;
import org.junit.ClassRule;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.kingbaseES.KingbaseOfficialTestDatabase;
import reactor.test.StepVerifier;

import java.util.List;

/** Upstream OracleIndexMetadataParserTest at 96b61b0b; assertions are unchanged. */
public class KingbaseOracleOfficialIndexMetadataTests {

    @ClassRule
    public static final KingbaseOfficialTestDatabase DATABASE = new KingbaseOfficialTestDatabase("oracle");

    @Test
    public void test() {

        SyncSqlExecutor sqlExecutor = DATABASE.sync();
        try {

            RDBSchemaMetadata schema = DATABASE.newSchema();
            schema.setDatabase(new RDBDatabaseMetadata(new KingbaseOracleDialect()));
            schema.addFeature(DATABASE.reactive());

            KingbaseOracleIndexMetadataParser parser = KingbaseOracleIndexMetadataParser.of(schema);

            schema.addFeature(sqlExecutor);

            sqlExecutor.execute(SqlRequests.of(
                    "create table test_index_parser(" +
                            "id varchar2(32) primary key," +
                            "name varchar2(32) not null," +
                            "age int," +
                            "addr varchar2(128))"));

            sqlExecutor.execute(SqlRequests.of("create index test_index_0 on test_index_parser (age)"));

            sqlExecutor.execute(SqlRequests.of("create unique index test_index_2 on test_index_parser (name)"));

            sqlExecutor.execute(SqlRequests.of("create index test_index_3 on test_index_parser (name,addr desc)"));

            List<RDBIndexMetadata> list = parser.parseTableIndex("test_index_parser");

            Assert.assertEquals(list.size(), 4);
            Assert.assertTrue(list.stream().anyMatch(RDBIndexMetadata::isPrimaryKey));

            parser.parseTableIndexReactive("test_index_parser")
                  .as(StepVerifier::create)
                  .expectNextCount(4)
                  .verifyComplete();

        } finally {
            try {
                sqlExecutor.execute(SqlRequests.of("drop table test_index_parser"));
            } catch (Exception e) {

            }
        }

    }
}