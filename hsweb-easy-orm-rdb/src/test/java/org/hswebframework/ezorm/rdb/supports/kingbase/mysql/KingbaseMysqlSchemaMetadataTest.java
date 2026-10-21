package org.hswebframework.ezorm.rdb.supports.kingbase.mysql;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateIndexParameter;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateIndexSqlBuilder;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;

/**
 * Verifies the schema-level MySQL-mode DDL exposed to EasyORM.
 */
public class KingbaseMysqlSchemaMetadataTest {

    @Test
    public void shouldUseKingbaseMysqlDialectAndCreateIdempotentIndexes() {
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
            "create index if not exists idx_account_id on `public`.account ( `id` asc )",
            request.getSql().replaceAll("\\s+", " ").trim()
        );
    }
}
