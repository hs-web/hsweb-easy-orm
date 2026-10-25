package org.hswebframework.ezorm.rdb.supports.kingbase.mysql;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.dml.QueryOperator;
import org.hswebframework.ezorm.rdb.operator.dml.query.SelectColumn;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateIndexParameter;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateIndexSqlBuilder;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;

/**
 * Verifies schema-level MySQL-mode SQL generation.
 */
public class KingbaseMysqlSchemaMetadataTest {

    @Test
    public void shouldUseKingbaseMysqlDialectAndCreateIndexes() {
        KingbaseMysqlSchemaMetadata schema = new KingbaseMysqlSchemaMetadata("public");
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(schema.getDialect());
        database.addSchema(schema);
        database.setCurrentSchema(schema);
        Assert.assertEquals("kingbase-mysql", schema.getDialect().getId());

        RDBTableMetadata table = schema.newTable("account");
        RDBColumnMetadata id = new RDBColumnMetadata();
        id.setName("id");
        id.setJdbcType(JDBCType.BIGINT, Long.class);
        table.addColumn(id);

        RDBIndexMetadata index = new RDBIndexMetadata();
        index.setName("idx_account_id");
        index.getColumns().add(RDBIndexMetadata.IndexColumn.of("id", RDBIndexMetadata.IndexSort.asc));

        CreateIndexSqlBuilder builder = schema.findFeatureNow(CreateIndexSqlBuilder.ID);
        SqlRequest request = builder.build(CreateIndexParameter.of(table, index));

        Assert.assertEquals(
            "create index idx_account_id on `public`.account ( `id` asc )",
            request.getSql().replaceAll("\\s+", " ").trim()
        );
    }

    @Test
    public void shouldRejectInsertWhenNoRequestedColumnExists() {
        KingbaseMysqlSchemaMetadata schema = new KingbaseMysqlSchemaMetadata("public");
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(schema.getDialect());
        database.addSchema(schema);
        database.setCurrentSchema(schema);

        RDBTableMetadata table = schema.newTable("account");
        RDBColumnMetadata id = new RDBColumnMetadata();
        id.setName("id");
        id.setJdbcType(JDBCType.BIGINT, Long.class);
        table.addColumn(id);
        schema.addTable(table);
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);

        System.out.println("[DML异常场景] 表=" + table.getFullName()
            + "，已有字段=id，仅指定不存在的插入字段=missing_column");
        IllegalArgumentException exception = Assert.assertThrows(
            IllegalArgumentException.class,
            () -> operator.dml()
                .insert("account")
                .columns("missing_column")
                .values("test")
                .getSql()
        );
        System.out.println("[实际异常] " + exception.getClass().getName() + ": " + exception.getMessage());
        Assert.assertEquals("No operable columns", exception.getMessage());
        System.out.println("[验证结果] 插入 SQL 生成失败，异常提示没有可操作字段，测试通过");
    }

    @Test
    public void shouldRejectPagingQueryWhenGroupByColumnDoesNotExist() {
        KingbaseMysqlSchemaMetadata schema = new KingbaseMysqlSchemaMetadata("public");
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(schema.getDialect());
        database.addSchema(schema);
        database.setCurrentSchema(schema);

        RDBTableMetadata table = schema.newTable("account");
        RDBColumnMetadata id = new RDBColumnMetadata();
        id.setName("id");
        id.setJdbcType(JDBCType.BIGINT, Long.class);
        table.addColumn(id);
        schema.addTable(table);
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);

        QueryOperator query = operator.dml()
            .query("account")
            .select("id")
            .groupBy(SelectColumn.of("missing_column"))
            .paging(0, 10);
        System.out.println("[查询分页异常] 表=" + table.getFullName()
            + "，已有字段=id，分组字段=missing_column，页码=0，每页=10");

        IllegalStateException exception = Assert.assertThrows(IllegalStateException.class, query::getSql);
        System.out.println("[实际异常] " + exception.getClass().getName() + ": " + exception.getMessage());
        Assert.assertEquals("column not found:missing_column in " + table.getFullName(), exception.getMessage());
        System.out.println("[验证结果] 查询 SQL 生成失败，异常提示分组字段不存在及所属表，测试通过");
    }
}
