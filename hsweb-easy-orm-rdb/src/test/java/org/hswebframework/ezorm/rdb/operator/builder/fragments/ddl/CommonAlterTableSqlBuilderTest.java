package org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl;

import org.hswebframework.ezorm.rdb.executor.BatchSqlRequest;
import org.hswebframework.ezorm.rdb.executor.EmptySqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.*;
import org.hswebframework.ezorm.rdb.operator.builder.MetadataHelper;
import org.hswebframework.ezorm.rdb.supports.h2.H2IndexMetadataParser;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class CommonAlterTableSqlBuilderTest {


    @Test
    public void testIndex() {
        RDBSchemaMetadata schema = MetadataHelper.createMockSchema();
        schema.addFeature(new H2IndexMetadataParser(schema));
        CommonAlterTableSqlBuilder builder = new CommonAlterTableSqlBuilder();

        RDBTableMetadata old = schema.getTable("test").orElseThrow(NullPointerException::new);

        RDBTableMetadata copy = old.clone();

        {
            RDBIndexMetadata index = new RDBIndexMetadata();
            index.setName("test_index");
            index.setAlias("test");
            index.setColumns(Collections.singletonList(RDBIndexMetadata.IndexColumn.of("name", RDBIndexMetadata.IndexSort.asc)));
            copy.addIndex(index);
            SqlRequest sqlRequest = builder.build(AlterRequest.builder()
                    .oldTable(old)
                    .newTable(copy)
                    .build());
            Assert.assertTrue(sqlRequest.isNotEmpty());

        }


    }

    @Test
    public void test() {
        RDBSchemaMetadata schema = MetadataHelper.createMockSchema();
        CommonAlterTableSqlBuilder builder = new CommonAlterTableSqlBuilder();

        RDBTableMetadata old = schema.getTable("test").orElseThrow(NullPointerException::new);

        RDBTableMetadata copy = old.clone();

        {
            RDBColumnMetadata test = new RDBColumnMetadata();
            test.setName("test");
            test.setJdbcType(JDBCType.VARCHAR, String.class);
            test.setLength(32);
            test.setComment("test");
            copy.addColumn(test);
            SqlRequest sql = builder.build(AlterRequest.builder()
                    .oldTable(old)
                    .newTable(copy)
                    .build());
            System.out.println(sql);
            List<SqlRequest> sqlList = ((BatchSqlRequest) sql).getBatch();
            Assert.assertEquals(sqlList.size(), 1);
            Assert.assertEquals(sql.getSql(), "alter table PUBLIC.test add \"TEST\" varchar(32)");
            Assert.assertEquals(sqlList.get(0).getSql(), "comment on column PUBLIC.test.\"TEST\" is 'test'");

        }

        {
            copy = old.clone();
            copy.getColumn("name").ifPresent(name -> name.setLength(200));

            SqlRequest sql = builder.build(AlterRequest.builder()
                    .allowAlter(true)
                    .oldTable(old)
                    .newTable(copy)
                    .build());
            Assert.assertEquals(sql.getSql(), "alter table PUBLIC.test modify \"NAME\" varchar(200) null");
        }

        {
            copy = old.clone();
            copy.removeColumn("name");
            SqlRequest sql = builder.build(AlterRequest.builder()
                    .oldTable(old)
                    .newTable(copy)
                    .allowDrop(true)
                    .build());

            Assert.assertEquals(sql.getSql(), "alter table PUBLIC.test drop column \"NAME\"");
        }
    }

}