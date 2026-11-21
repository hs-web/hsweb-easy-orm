package org.hswebframework.ezorm.rdb.supports.kingbaseES.mysql;

import org.hswebframework.ezorm.rdb.executor.BatchSqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateTableSqlBuilder;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlCreateTableSqlBuilder;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.sql.JDBCType;

public class KingbaseMysqlCreateTableSqlBuilderTest {

    private KingbaseMysqlSchemaMetadata schema;

    @Before
    public void setUp() {
        schema = new KingbaseMysqlSchemaMetadata("public");
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(schema.getDialect());
        database.addSchema(schema);
        database.setCurrentSchema(schema);
    }

    @Test
    public void shouldReuseNativeTableOptionsCommentsAndIndexes() {
        RDBTableMetadata table = schema.newTable("device_info");
        table.setComment("device table");

        RDBColumnMetadata id = table.newColumn();
        id.setName("id");
        id.setJdbcType(JDBCType.INTEGER, Integer.class);
        id.setPrimaryKey(true);
        id.setAutoIncrement(true);
        table.addColumn(id);

        RDBColumnMetadata name = table.newColumn();
        name.setName("name");
        name.setJdbcType(JDBCType.VARCHAR, String.class);
        name.setLength(64);
        name.setNotNull(true);
        name.setComment("device name");
        table.addColumn(name);

        RDBIndexMetadata index = new RDBIndexMetadata("idx_device_name");
        index.setUnique(true);
        index.getColumns().add(RDBIndexMetadata.IndexColumn.of("name", RDBIndexMetadata.IndexSort.asc));
        table.addIndex(index);

        CreateTableSqlBuilder builder = schema.findFeatureNow(CreateTableSqlBuilder.ID);
        Assert.assertTrue(builder instanceof MysqlCreateTableSqlBuilder);
        BatchSqlRequest request = (BatchSqlRequest) builder.build(table);
        String sql = request.getSql().replaceAll("\\s+", " ").trim();
        Assert.assertTrue(sql, sql.contains("`id` int not null primary key auto_increment"));
        Assert.assertTrue(sql, sql.contains("`name` varchar(64) not null comment 'device name'"));
        Assert.assertTrue(sql, sql.contains("ENGINE= InnoDB DEFAULT CHARSET= utf8mb4"));
        Assert.assertTrue(sql, sql.contains("COMMENT= 'device table'"));
        Assert.assertEquals(1, request.getBatch().size());
        Assert.assertEquals("create unique index idx_device_name on `public`.device_info ( `name` asc )",
            request.getBatch().get(0).getSql().replaceAll("\\s+", " ").trim());
    }

    @Test
    public void shouldRetainNativeEngineAndCharsetConfiguration() {
        KingbaseMysqlCreateTableSqlBuilder builder = (KingbaseMysqlCreateTableSqlBuilder)
            schema.findFeatureNow(CreateTableSqlBuilder.ID);
        builder.setEngine("MyISAM");
        builder.setCharset("latin1");

        RDBTableMetadata table = schema.newTable("configured_table");
        RDBColumnMetadata id = table.newColumn();
        id.setName("id");
        id.setJdbcType(JDBCType.INTEGER, Integer.class);
        table.addColumn(id);

        String sql = builder.build(table).getSql().replaceAll("\\s+", " ").trim();
        Assert.assertTrue(sql, sql.contains("ENGINE= MyISAM DEFAULT CHARSET= latin1"));
    }
}
