package org.hswebframework.ezorm.rdb.supports.kingbaseES.oracle;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CommonCreateIndexSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateIndexParameter;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateIndexSqlBuilder;
import org.hswebframework.ezorm.rdb.supports.oracle.OracleSchemaMetadata;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.sql.JDBCType;

public class KingbaseOracleCreateIndexSqlBuilderTest {

    private RDBTableMetadata table;
    private CreateIndexSqlBuilder builder;
    private CreateIndexSqlBuilder nativeBuilder;

    @Before
    public void setUp() {
        KingbaseOracleSchemaMetadata schema = new KingbaseOracleSchemaMetadata("public");
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(schema.getDialect());
        database.addSchema(schema);
        database.setCurrentSchema(schema);
        table = schema.newTable("ACCOUNT");
        for (String name : new String[]{"NAME", "CATEGORY"}) {
            RDBColumnMetadata column = table.newColumn();
            column.setName(name);
            column.setJdbcType(JDBCType.VARCHAR, String.class);
            column.setLength(64);
            table.addColumn(column);
        }
        builder = schema.findFeatureNow(CreateIndexSqlBuilder.ID);
        nativeBuilder = new OracleSchemaMetadata("public").findFeatureNow(CreateIndexSqlBuilder.ID);
    }

    @Test
    public void shouldMatchNativeOracleOrdinaryUniqueAndCompositeIndexes() {
        Assert.assertTrue(builder instanceof CommonCreateIndexSqlBuilder);
        for (int kind = 0; kind < 3; kind++) {
            RDBIndexMetadata index = new RDBIndexMetadata("idx_account_name");
            index.setUnique(kind == 1);
            index.getColumns().add(RDBIndexMetadata.IndexColumn.of("NAME", RDBIndexMetadata.IndexSort.asc));
            if (kind == 2) {
                index.getColumns().add(RDBIndexMetadata.IndexColumn.of("CATEGORY", RDBIndexMetadata.IndexSort.desc));
            }
            CreateIndexParameter parameter = CreateIndexParameter.of(table, index);
            SqlRequest request = builder.build(parameter);
            Assert.assertEquals(nativeBuilder.build(parameter).getSql(), request.getSql());
            Assert.assertEquals("create " + (kind == 1 ? "unique " : "")
                    + "index idx_account_name on \"public\".ACCOUNT ( \"NAME\" asc"
                    + (kind == 2 ? " , \"CATEGORY\" desc" : "") + " )",
                request.getSql().replaceAll("\\s+", " ").trim());
            System.out.println("[索引生成与原生一致] " + request.getSql());
        }
    }

    @Test
    public void shouldMatchNativeOracleMissingColumnException() {
        RDBIndexMetadata index = new RDBIndexMetadata("idx_missing");
        index.getColumns().add(RDBIndexMetadata.IndexColumn.of("MISSING", RDBIndexMetadata.IndexSort.asc));
        CreateIndexParameter parameter = CreateIndexParameter.of(table, index);

        UnsupportedOperationException expected = Assert.assertThrows(
            UnsupportedOperationException.class, () -> nativeBuilder.build(parameter));
        UnsupportedOperationException actual = Assert.assertThrows(
            UnsupportedOperationException.class, () -> builder.build(parameter));
        Assert.assertEquals(expected.getMessage(), actual.getMessage());
    }
}
