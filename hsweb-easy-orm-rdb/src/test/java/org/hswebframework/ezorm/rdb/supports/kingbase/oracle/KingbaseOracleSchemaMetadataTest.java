package org.hswebframework.ezorm.rdb.supports.kingbase.oracle;

import org.hswebframework.ezorm.rdb.executor.BatchSqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateTableSqlBuilder;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;

/**
 * Verifies that KingbaseES Oracle mode registers its DDL behavior on the
 * public schema metadata API.
 */
public class KingbaseOracleSchemaMetadataTest {

    @Test
    public void shouldRenderOracleCompatibleCreateTableSql() {
        KingbaseOracleDialect dialect = new KingbaseOracleDialect();
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(dialect);
        KingbaseOracleSchemaMetadata schema = new KingbaseOracleSchemaMetadata("public");
        database.addSchema(schema);
        database.setCurrentSchema(schema);

        RDBTableMetadata table = schema.newTable("ACCOUNT");

        RDBColumnMetadata id = new RDBColumnMetadata();
        id.setName("ID");
        id.setJdbcType(JDBCType.NUMERIC, Long.class);
        id.setPrecision(20);
        id.setPrimaryKey(true);
        id.setNotNull(true);
        table.addColumn(id);

        RDBColumnMetadata name = new RDBColumnMetadata();
        name.setName("NAME");
        name.setJdbcType(JDBCType.VARCHAR, String.class);
        name.setLength(64);
        name.setNotNull(true);
        table.addColumn(name);

        SqlRequest request = schema.findFeatureNow(CreateTableSqlBuilder.ID).build(table);
        System.out.println("[DDL正常场景] 为 ACCOUNT 表生成建表 SQL，包含主键、字段类型和非空约束");
        System.out.println("[生成SQL] " + request.getSql().replaceAll("\\s+", " ").trim());

        Assert.assertTrue(request instanceof BatchSqlRequest);
        Assert.assertEquals(
            "create table \"public\".ACCOUNT ( \"ID\" number(20,0) not null primary key , \"NAME\" varchar2(64 char) not null )",
            request.getSql().replaceAll("\\s+", " ").trim()
        );
    }

    @Test
    public void shouldRejectCreateTableWhenColumnTypeIsMissing() {
        KingbaseOracleSchemaMetadata schema = new KingbaseOracleSchemaMetadata("public");
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(schema.getDialect());
        database.addSchema(schema);
        database.setCurrentSchema(schema);

        RDBTableMetadata table = schema.newTable("ACCOUNT");
        RDBColumnMetadata column = new RDBColumnMetadata();
        column.setName("NAME");
        table.addColumn(column);

        System.out.println("[DDL异常场景] 表=" + table.getFullName() + "，字段=NAME，未指定字段类型");
        UnsupportedOperationException exception = Assert.assertThrows(
            UnsupportedOperationException.class,
            () -> schema.findFeatureNow(CreateTableSqlBuilder.ID).build(table)
        );
        System.out.println("[实际异常] " + exception.getClass().getName() + ": " + exception.getMessage());
        Assert.assertTrue(exception.getMessage().startsWith("unknown column type : "));
        Assert.assertTrue(exception.getMessage().contains("NAME"));
        System.out.println("[验证结果] 建表 SQL 生成失败，异常提示字段类型未知，测试通过");
    }

    @Test
    public void shouldRetainAllCommentsAfterCreateTableStatement() {
        KingbaseOracleSchemaMetadata schema = new KingbaseOracleSchemaMetadata("public");
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(schema.getDialect());
        database.addSchema(schema);
        database.setCurrentSchema(schema);

        RDBTableMetadata table = schema.newTable("ACCOUNT");
        table.setComment("Account table");

        RDBColumnMetadata name = new RDBColumnMetadata();
        name.setName("NAME");
        name.setJdbcType(JDBCType.VARCHAR, String.class);
        name.setLength(64);
        name.setComment("Account name");
        table.addColumn(name);

        BatchSqlRequest request = (BatchSqlRequest) schema
            .findFeatureNow(CreateTableSqlBuilder.ID)
            .build(table);

        Assert.assertTrue(request.getSql().startsWith("create table"));
        Assert.assertEquals(2, request.getBatch().size());
        Assert.assertTrue(request.getBatch().get(0).getSql().startsWith("comment on column"));
        Assert.assertTrue(request.getBatch().get(1).getSql().startsWith("comment on table"));
    }
}
