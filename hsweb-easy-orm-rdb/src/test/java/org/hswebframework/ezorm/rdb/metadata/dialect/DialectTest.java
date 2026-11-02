package org.hswebframework.ezorm.rdb.metadata.dialect;

import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.supports.kingbase.mysql.KingbaseMysqlDialect;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.opengauss.OpengaussDialect;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;
import java.util.Arrays;

public class DialectTest {

    @Test
    public void test() {
        Dialect dialect = Dialect.H2;

        Assert.assertEquals(dialect.quote("name"), "\"NAME\"");

        Assert.assertEquals(dialect.clearQuote("\"NAME\""), "NAME");

        Assert.assertEquals(dialect.clearQuote("test.\"NAME\""), "test.NAME");

        Assert.assertEquals(dialect.clearQuote("db1.test.\"NAME\""), "db1.test.NAME");

    }

    @Test
    public void testMysqlEnumDataType() {
        assertMysqlLiteralEnumType(Dialect.MYSQL.convertDataType("enum('Day','Month')"));
        assertMysqlLiteralEnumType(new KingbaseMysqlDialect().convertDataType("enum('Day','Month')"));
    }

    @Test
    public void testMysqlNumericDataTypeCaseInsensitive() {
        MysqlSchemaMetadata schema = new MysqlSchemaMetadata("test");
        RDBTableMetadata table = schema.newTable("test");
        RDBColumnMetadata column = table.newColumn();
        column.setName("amount");
        column.setType(Dialect.MYSQL.convertDataType("DECIMAL(10,2)"));
        table.addColumn(column);

        Assert.assertEquals("decimal(10,2)", column.getDataType());
        Assert.assertEquals(10, column.getPrecision());
        Assert.assertEquals(2, column.getScale());
        Assert.assertEquals(JDBCType.DECIMAL, column.getSqlType());
    }

    @Test
    public void testLikeAndConcat() {
        SqlFragments parameter = PrepareSqlFragments.of()
            .add(SqlFragments.QUESTION_MARK)
            .addParameter("value");

        SqlFragments like = Dialect.H2.buildLike(
            SqlFragments.of("name"),
            parameter,
            false,
            true
        );
        Assert.assertEquals("lower( name ) like lower( ? )", like.toRequest().getSql());
        Assert.assertEquals("value", like.getParameters().get(0));

        Assert.assertEquals(
            "name",
            Dialect.ORACLE.buildConcat(SqlFragments.of("name"))
                         .toRequest()
                         .getSql()
        );
        Assert.assertEquals(
            "concat( concat( '%' , name ) , '%' )",
            Dialect.ORACLE.buildConcat(
                              SqlFragments.of("'%'"),
                              SqlFragments.of("name"),
                              SqlFragments.of("'%'"))
                         .toRequest()
                         .getSql()
        );
    }

    @Test
    public void testDialectLikeOperators() {
        Assert.assertEquals(
            "name ilike ?",
            Dialect.POSTGRES.buildLike(SqlFragments.of("name"), parameter(), false, true)
                             .toRequest()
                             .getSql()
        );
        Assert.assertEquals(
            "name not ilike ?",
            new OpengaussDialect().buildLike(SqlFragments.of("name"), parameter(), true, true)
                                  .toRequest()
                                  .getSql()
        );
        Assert.assertEquals(
            "name like ?",
            Dialect.POSTGRES.buildLike(SqlFragments.of("name"), parameter(), false, false)
                             .toRequest()
                             .getSql()
        );

        for (Dialect dialect : Arrays.asList(
            Dialect.H2,
            Dialect.MYSQL,
            Dialect.ORACLE,
            Dialect.MSSQL,
            Dialect.KINGBASE_MYSQL
        )) {
            Assert.assertEquals(
                dialect.getName(),
                "lower( name ) like lower( ? )",
                dialect.buildLike(SqlFragments.of("name"), parameter(), false, true)
                       .toRequest()
                       .getSql()
            );
        }
    }

    @Test
    public void testEmptyLikeExpressions() {
        Assert.assertTrue(Dialect.H2.buildLower(null).isEmpty());
        Assert.assertTrue(Dialect.H2.buildLower(SqlFragments.of()).isEmpty());
        Assert.assertTrue(Dialect.H2.buildConcat((SqlFragments[]) null).isEmpty());
        Assert.assertTrue(Dialect.H2.buildConcat().isEmpty());
    }

    private void assertMysqlLiteralEnumType(DataType type) {
        MysqlSchemaMetadata schema = new MysqlSchemaMetadata("test");
        RDBTableMetadata table = schema.newTable("test");
        RDBColumnMetadata column = table.newColumn();
        column.setName("status");
        column.setType(type);
        table.addColumn(column);

        Assert.assertEquals("enum('Day','Month')", column.getDataType());
        Assert.assertEquals(JDBCType.VARCHAR, column.getSqlType());
        Assert.assertEquals(String.class, column.getJavaType());
        Assert.assertFalse(column.getType().isLengthSupport());
    }

    private SqlFragments parameter() {
        return PrepareSqlFragments.of()
            .add(SqlFragments.QUESTION_MARK)
            .addParameter("value");
    }

}
