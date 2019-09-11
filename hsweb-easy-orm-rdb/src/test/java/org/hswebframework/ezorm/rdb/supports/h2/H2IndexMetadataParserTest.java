package org.hswebframework.ezorm.rdb.supports.h2;

import org.hswebframework.ezorm.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.junit.Assert;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.*;

public class H2IndexMetadataParserTest {

    @Test
    public void test() {
        SyncSqlExecutor sqlExecutor = new TestSyncSqlExecutor(new H2ConnectionProvider());
        sqlExecutor.execute(SqlRequests.of(
                "create table test_index_parser(" +
                        "id varchar(32) primary key," +
                        "name varchar(32) not null," +
                        "age number(4)," +
                        "addr varchar(128))"));


        sqlExecutor.execute(SqlRequests.of("create index test_index on test_index_parser (age)"));

        sqlExecutor.execute(SqlRequests.of("create unique index test_index_2 on test_index_parser (name)"));

        sqlExecutor.execute(SqlRequests.of("create index test_index_3 on test_index_parser (name,addr desc)"));

        H2SchemaMetadata schema = new H2SchemaMetadata("PUBLIC");
        schema.addFeature(sqlExecutor);

        H2IndexMetadataParser parser = H2IndexMetadataParser.of(schema);

        List<RDBIndexMetadata> index = parser.parseTableIndex("test_index_parser");

        index.forEach(System.out::println);

        Assert.assertNotNull(index);
        Assert.assertEquals(index.size(), 4);

        {
            RDBIndexMetadata id = index.stream()
                    .filter(meta -> meta.getColumns().stream()
                            .anyMatch(indexColumn -> indexColumn.getColumn().equals("id")))
                    .findFirst().orElseThrow(NullPointerException::new);
            Assert.assertTrue(id.isPrimaryKey());
            Assert.assertTrue(id.isUnique());
        }

        {
            RDBIndexMetadata nameAndAddr = index.stream()
                    .filter(meta -> meta.getName().equals("test_index_3"))
                    .findFirst().orElseThrow(NullPointerException::new);
            Assert.assertFalse(nameAndAddr.isPrimaryKey());
            Assert.assertFalse(nameAndAddr.isUnique());

            Assert.assertEquals(nameAndAddr.getColumns().size(),2);

            Assert.assertEquals(nameAndAddr.getColumns().get(0).getSort(), RDBIndexMetadata.IndexSort.asc);
            Assert.assertEquals(nameAndAddr.getColumns().get(1).getSort(), RDBIndexMetadata.IndexSort.desc);

        }


    }

}