package org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl;

import org.hswebframework.ezorm.rdb.executor.BatchSqlRequest;
import org.hswebframework.ezorm.rdb.executor.EmptySqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.MetadataHelper;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;
import java.util.List;

public class CommonAlterTableSqlBuilderTest {


    @Test
    public void test() {
        RDBSchemaMetadata schema = MetadataHelper.createMockSchema();
        CommonAlterTableSqlBuilder builder = new CommonAlterTableSqlBuilder();

        RDBTableMetadata old = schema.getTable("test").orElseThrow(NullPointerException::new);

        RDBTableMetadata copy = old.clone();

        {
            RDBColumnMetadata test = new RDBColumnMetadata();
            test.setName("test");
            test.setJdbcType(JDBCType.VARCHAR);
            test.setLength(32);
            test.setComment("test");
            copy.addColumn(test);
            SqlRequest sql = builder.build(AlterRequest.builder()
                    .oldTable(old)
                    .newTable(copy)
                    .build());
            List<SqlRequest> sqlList = ((BatchSqlRequest) sql).getBatch();
            Assert.assertEquals(sqlList.size(),2);
            Assert.assertEquals(sqlList.get(0).getSql(), "alter table PUBLIC.test add \"TEST\" varchar(32)");
            Assert.assertEquals(sqlList.get(1).getSql(), "comment on column test.\"TEST\" is 'test'");

        }

        {
            copy = old.clone();
            copy.getColumn("name").ifPresent(name->name.setLength(200));

            SqlRequest sql = builder.build(AlterRequest.builder()
                    .oldTable(old)
                    .newTable(copy)
                    .build());
            List<SqlRequest> sqlList = ((BatchSqlRequest) sql).getBatch();
            Assert.assertEquals(sqlList.get(0).getSql(), "alter table PUBLIC.test modify \"NAME\" varchar(200) null");
        }

        {
            copy = old.clone();
            copy.removeColumn("name");
            SqlRequest sql = builder.build(AlterRequest.builder()
                    .oldTable(old)
                    .newTable(copy)
                    .allowDrop(true)
                    .build());
            List<SqlRequest> sqlList = ((BatchSqlRequest) sql).getBatch();
            Assert.assertEquals(sqlList.get(0).getSql(), "alter table PUBLIC.test drop column \"NAME\"");
        }
    }

}