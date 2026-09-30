package org.hswebframework.ezorm.rdb.operator.builder.fragments.function;

import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.dml.query.SelectColumn;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlSchemaMetadata;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.sql.JDBCType;

public class CountFunctionFragmentBuilderTest {

    private RDBDatabaseMetadata database;
    private PostgresqlSchemaMetadata schema;
    private RDBTableMetadata table;

    @Before
    public void init() {
        database = new RDBDatabaseMetadata(Dialect.POSTGRES);
        schema = new PostgresqlSchemaMetadata("public");
        database.addSchema(schema);
        database.setCurrentSchema(schema);

        table = schema.newTable("metrics");
        addColumn("id", true);
        addColumn("value", false);
        schema.addTable(table);
    }

    @Test
    public void shouldOnlyRewriteRequestedNonNullCount() {
        String normalSql = querySql(count("id", "total"));
        Assert.assertTrue(normalSql, normalSql.contains("count( " + table.getColumnNow("id").getFullName() + " ) as \"total\""));
        Assert.assertFalse(normalSql, normalSql.contains("count(*)"));

        SelectColumn countRows = count("id", "total");
        countRows.option(CountFunctionFragmentBuilder.COUNT_ROWS, true);
        String rowsSql = querySql(countRows);
        Assert.assertTrue(rowsSql, rowsSql.contains("count(*) as \"total\""));
    }

    @Test
    public void shouldKeepNullableColumnCount() {
        SelectColumn countRows = count("value", "total");
        countRows.option(CountFunctionFragmentBuilder.COUNT_ROWS, true);

        String sql = querySql(countRows);
        Assert.assertTrue(sql, sql.contains("count( " + table.getColumnNow("value").getFullName() + " ) as \"total\""));
        Assert.assertFalse(sql, sql.contains("count(*)"));
    }

    @Test
    public void shouldKeepDistinctAndArgOptions() {
        SelectColumn distinct = count("id", "total");
        distinct.option(CountFunctionFragmentBuilder.COUNT_ROWS, true);
        distinct.option("distinct", true);
        String distinctSql = querySql(distinct);
        Assert.assertTrue(distinctSql, distinctSql.contains("count( distinct " + table.getColumnNow("id").getFullName() + " )"));

        SelectColumn arg = count("id", "total");
        arg.option(CountFunctionFragmentBuilder.COUNT_ROWS, true);
        arg.option("arg", 1);
        String argSql = querySql(arg);
        Assert.assertTrue(argSql, argSql.contains("count( 1 ) as \"total\""));
    }

    @Test
    public void shouldRespectCustomCountFunction() {
        schema.addFeature(new SimpleFunctionFragmentBuilder("count", "schema_count", "自定义计数"));
        SelectColumn countRows = count("id", "total");
        countRows.option(CountFunctionFragmentBuilder.COUNT_ROWS, true);
        String schemaSql = querySql(countRows);
        Assert.assertTrue(schemaSql, schemaSql.contains("schema_count( " + table.getColumnNow("id").getFullName() + " )"));

        table.addFeature(new SimpleFunctionFragmentBuilder("count", "table_count", "自定义计数"));
        String tableSql = querySql(countRows);
        Assert.assertTrue(tableSql, tableSql.contains("table_count( " + table.getColumnNow("id").getFullName() + " )"));
    }

    @Test
    public void shouldKeepLegacyBuilderBehaviorForUnknownOption() {
        schema.addFeature(new SimpleFunctionFragmentBuilder("count", "计数"));
        SelectColumn countRows = count("id", "total");
        countRows.option(CountFunctionFragmentBuilder.COUNT_ROWS, true);

        String sql = querySql(countRows);
        Assert.assertTrue(sql, sql.contains("count( " + table.getColumnNow("id").getFullName() + " )"));
        Assert.assertFalse(sql, sql.contains("count(*)"));
    }

    private String querySql(SelectColumn column) {
        return DefaultDatabaseOperator.of(database)
                                      .dml()
                                      .query("metrics")
                                      .select(column)
                                      .getSql()
                                      .getSql();
    }

    private SelectColumn count(String property, String alias) {
        SelectColumn column = SelectColumn.of(property, alias);
        column.setFunction("count");
        return column;
    }

    private void addColumn(String name, boolean notNull) {
        RDBColumnMetadata column = table.newColumn();
        column.setName(name);
        column.setType(JdbcDataType.of(JDBCType.VARCHAR, String.class));
        column.setNotNull(notNull);
        table.addColumn(column);
    }
}
