package org.hswebframework.ezorm.rdb.supports.kingbaseES.mssql;

import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.supports.mssql.SqlServerDialect;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;

public class KingbaseSqlServerDialectTest {

    private final KingbaseSqlServerDialect dialect = new KingbaseSqlServerDialect();

    @Test
    public void shouldInheritSqlServerQuotingAndTypeRendering() {
        Assert.assertTrue(dialect instanceof SqlServerDialect);
        Assert.assertEquals("kingbase-sqlserver", dialect.getId());
        Assert.assertEquals("KingbaseSqlServer", dialect.getName());
        Assert.assertEquals("[account]", dialect.quote("account"));
        Assert.assertEquals("dbo.account", dialect.clearQuote("[dbo].[account]"));
        Assert.assertFalse(dialect.isColumnToUpperCase());

        JDBCType[] types = {JDBCType.VARCHAR, JDBCType.TIMESTAMP, JDBCType.BOOLEAN,
            JDBCType.BLOB, JDBCType.INTEGER, JDBCType.DECIMAL};
        String[] expected = {"nvarchar(64)", "datetime2", "bit", "varbinary(max)", "int", "numeric(18,2)"};
        for (int i = 0; i < types.length; i++) {
            RDBColumnMetadata column = new RDBColumnMetadata();
            column.setJdbcType(types[i], Object.class);
            column.setLength(64);
            column.setPrecision(18);
            column.setScale(2);
            Assert.assertEquals(expected[i], dialect.buildColumnDataType(column));
        }
    }

    @Test
    public void shouldRecognizeKingbaseCatalogTypeAliases() {
        String[] types = {"character varying(64)", "integer", "timestamp without time zone", "bytea"};
        JDBCType[] expected = {JDBCType.VARCHAR, JDBCType.INTEGER, JDBCType.TIMESTAMP, JDBCType.VARBINARY};
        for (int i = 0; i < types.length; i++) {
            Assert.assertEquals(types[i], expected[i], dialect.convertDataType(types[i]).getSqlType());
        }
    }

    // Same SQL and parameter contract as metadata/dialect/DialectTest.testDialectLikeOperators.
    @Test
    public void shouldRetainOfficialCaseInsensitiveLikeContract() {
        SqlFragments value = PrepareSqlFragments.of().add(SqlFragments.QUESTION_MARK).addParameter("value");
        SqlFragments like = dialect.buildLike(SqlFragments.of("name"), value, false, true);
        Assert.assertEquals("lower( name ) like lower( ? )", like.toRequest().getSql());
        Assert.assertEquals("value", like.getParameters().get(0));
    }

    @Test
    public void shouldPreserveMaxTypeDefinitionsReturnedByKingbase() {
        for (String type : new String[]{"varchar(max)", "nvarchar(MAX)", "varbinary(max)"}) {
            RDBColumnMetadata column = new RDBColumnMetadata();
            column.setType(dialect.convertDataType(type));
            Assert.assertEquals(type.toLowerCase(java.util.Locale.ROOT), dialect.buildColumnDataType(column));
        }
    }
}
