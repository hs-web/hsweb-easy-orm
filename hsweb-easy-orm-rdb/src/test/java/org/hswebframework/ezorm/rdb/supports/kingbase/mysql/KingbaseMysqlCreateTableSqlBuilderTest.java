package org.hswebframework.ezorm.rdb.supports.kingbase.mysql;

import org.hswebframework.ezorm.core.DefaultValue;
import org.hswebframework.ezorm.rdb.executor.BatchSqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateIndexParameter;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateIndexSqlBuilder;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;
import java.util.concurrent.atomic.AtomicBoolean;

public class KingbaseMysqlCreateTableSqlBuilderTest {

    @Test
    public void testBuildCreateTableWithKingbaseCompatibleCommentsAndIndex() {
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.KINGBASE_MYSQL);
        KingbaseMysqlSchemaMetadata schema = new KingbaseMysqlSchemaMetadata("PUBLIC");
        database.addSchema(schema);
        database.setCurrentSchema(schema);
        RDBTableMetadata table = schema.newTable("device_info");
        table.setComment("device table");

        RDBColumnMetadata id = column("id");
        id.setPrimaryKey(true);
        id.setAutoIncrement(true);
        table.addColumn(id);

        RDBColumnMetadata name = column("name");
        name.setNotNull(true);
        name.setComment("device name");
        name.setDefaultValue(new NativeDefaultValue("'unknown'"));
        table.addColumn(name);

        RDBColumnMetadata state = column("state");
        state.setColumnDefinition("int default 0");
        state.setComment("device state");
        table.addColumn(state);

        RDBIndexMetadata index = new RDBIndexMetadata("idx_device_name");
        index.setUnique(true);
        index.getColumns().add(RDBIndexMetadata.IndexColumn.of("name", RDBIndexMetadata.IndexSort.asc));
        table.addIndex(index);

        AtomicBoolean indexBuilt = new AtomicBoolean();
        table.addFeature(new CreateIndexSqlBuilder() {
            @Override
            public SqlRequest build(CreateIndexParameter parameter) {
                indexBuilt.set(true);
                Assert.assertSame(table, parameter.getTable());
                Assert.assertSame(index, parameter.getIndex());
                return org.hswebframework.ezorm.rdb.executor.SqlRequests.of("create unique index idx_device_name on device_info(name)");
            }
        });

        BatchSqlRequest request = (BatchSqlRequest) new KingbaseMysqlCreateTableSqlBuilder().build(table);

        Assert.assertTrue(request.getSql().startsWith("create table"));
        Assert.assertTrue(request.getSql().contains("device_info"));
        Assert.assertTrue(request.getSql().contains("`id` varchar(64) not null primary key auto_increment"));
        Assert.assertTrue(request.getSql().contains("`name` varchar(64) not null default 'unknown'"));
        Assert.assertTrue(request.getSql().contains("`state` int default 0"));
        Assert.assertFalse(request.getSql().contains("ENGINE="));
        Assert.assertFalse(request.getSql().contains("DEFAULT CHARSET"));
        Assert.assertTrue(request.toString().contains("comment on column"));
        Assert.assertTrue(request.toString().contains("device name"));
        Assert.assertTrue(request.toString().contains("device state"));
        Assert.assertTrue(request.toString().contains("comment on table"));
        Assert.assertTrue(request.toString().contains("device table"));
        Assert.assertTrue(request.toString().contains("create unique index idx_device_name"));
        Assert.assertTrue(indexBuilt.get());
    }

    private static class NativeDefaultValue implements DefaultValue, NativeSql {
        private final String sql;

        private NativeDefaultValue(String sql) {
            this.sql = sql;
        }

        @Override
        public Object get() {
            return sql;
        }

        @Override
        public String getSql() {
            return sql;
        }
    }

    private RDBColumnMetadata column(String name) {
        RDBColumnMetadata column = new RDBColumnMetadata();
        column.setName(name);
        column.setLength(64);
        column.setType(JdbcDataType.of(JDBCType.VARCHAR, String.class));
        return column;
    }
}
