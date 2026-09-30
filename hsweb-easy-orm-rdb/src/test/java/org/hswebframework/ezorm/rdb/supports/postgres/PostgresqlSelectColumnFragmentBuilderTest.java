package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatures;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.metadata.key.ForeignKeyBuilder;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.function.SimpleFunctionFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.Join;
import org.hswebframework.ezorm.rdb.operator.dml.JoinType;
import org.hswebframework.ezorm.rdb.operator.dml.query.QueryOperatorParameter;
import org.hswebframework.ezorm.rdb.operator.dml.query.SelectColumn;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.sql.JDBCType;
import java.util.Collections;

public class PostgresqlSelectColumnFragmentBuilderTest {

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
        addColumn(table, "id", true);
        addColumn(table, "value", false);
        schema.addTable(table);
    }

    @Test
    public void shouldUseCountStarForNonNullColumnAndKeepAlias() {
        String sql = querySql(database, "metrics", count("id", "total"));

        Assert.assertTrue(sql, sql.contains("count(*) as \"total\""));

        String defaultAliasSql = querySql(database, "metrics", count("id", null));
        Assert.assertTrue(defaultAliasSql, defaultAliasSql.contains("count(*) as \"id\""));
    }

    @Test
    public void shouldKeepCountColumnForNullableColumnAndDistinctCount() {
        String nullableSql = querySql(database, "metrics", count("value", "total"));
        Assert.assertTrue(nullableSql, nullableSql.contains("count( " + table.getColumnNow("value").getFullName() + " ) as \"total\""));

        SelectColumn distinct = count("id", "uniqueId");
        distinct.option("distinct", true);
        String distinctSql = querySql(database, "metrics", distinct);
        Assert.assertTrue(distinctSql, distinctSql.contains("count( distinct " + table.getColumnNow("id").getFullName() + " ) as \"uniqueId\""));
        Assert.assertFalse(distinctSql, distinctSql.contains("count(*)"));
    }

    @Test
    public void shouldUseCustomCountFunctionFromSchemaOrTable() {
        schema.addFeature(new SimpleFunctionFragmentBuilder("count", "schema_count", "自定义计数"));
        String schemaSql = querySql(database, "metrics", count("id", "total"));
        Assert.assertTrue(schemaSql, schemaSql.contains("schema_count( " + table.getColumnNow("id").getFullName() + " ) as \"total\""));
        Assert.assertFalse(schemaSql, schemaSql.contains("count(*)"));

        table.addFeature(new SimpleFunctionFragmentBuilder("count", "table_count", "自定义计数"));
        String tableSql = querySql(database, "metrics", count("id", "total"));
        Assert.assertTrue(tableSql, tableSql.contains("table_count( " + table.getColumnNow("id").getFullName() + " ) as \"total\""));
        Assert.assertFalse(tableSql, tableSql.contains("count(*)"));
    }

    @Test
    public void shouldKeepCountColumnWhenQueryHasJoinOrLogicalForeignKey() {
        QueryOperatorParameter parameter = new QueryOperatorParameter();
        parameter.setSelect(Collections.singletonList(count("id", "total")));
        Join join = new Join();
        join.setType(JoinType.right);
        join.setTarget("details");
        parameter.getJoins().add(join);

        String joinedSql = table.findFeatureNow(RDBFeatures.select).createFragments(parameter).toRequest().getSql();
        Assert.assertTrue(joinedSql, joinedSql.contains("count( " + table.getColumnNow("id").getFullName() + " )"));
        Assert.assertFalse(joinedSql, joinedSql.contains("count(*)"));

        parameter.getJoins().clear();
        RDBTableMetadata details = schema.newTable("details");
        addColumn(details, "metric_id", false);
        schema.addTable(details);
        table.addForeignKey(ForeignKeyBuilder.builder().target("details").autoJoin(true).build().addColumn("id", "metric_id"));

        String foreignKeySql = table.findFeatureNow(RDBFeatures.select).createFragments(parameter).toRequest().getSql();
        Assert.assertTrue(foreignKeySql, foreignKeySql.contains("count( " + table.getColumnNow("id").getFullName() + " )"));
        Assert.assertFalse(foreignKeySql, foreignKeySql.contains("count(*)"));
    }

    @Test
    public void shouldRegisterBuilderWhenTableIsCreated() {
        RDBTableMetadata created = schema.newTable("new_metrics");
        addColumn(created, "id", true);

        QueryOperatorParameter parameter = new QueryOperatorParameter();
        parameter.setSelect(Collections.singletonList(count("id", "total")));
        String sql = created.findFeatureNow(RDBFeatures.select).createFragments(parameter).toRequest().getSql();
        Assert.assertTrue(sql, sql.contains("count(*) as \"total\""));
    }

    @Test
    public void shouldRegisterBuilderForLoadedTable() {
        RDBTableMetadata loaded = new RDBTableMetadata("loaded_metrics");
        schema.addTable(loaded);
        addColumn(loaded, "id", true);

        String sql = querySql(database, "loaded_metrics", count("id", "total"));
        Assert.assertTrue(sql, sql.contains("count(*) as \"total\""));
    }

    @Test
    public void shouldLeaveOtherDialectsUnchanged() {
        RDBDatabaseMetadata h2Database = new RDBDatabaseMetadata(Dialect.H2);
        RDBSchemaMetadata h2Schema = new RDBSchemaMetadata("PUBLIC");
        h2Database.addSchema(h2Schema);
        h2Database.setCurrentSchema(h2Schema);
        RDBTableMetadata h2Table = h2Schema.newTable("metrics");
        addColumn(h2Table, "id", true);
        h2Schema.addTable(h2Table);

        String sql = querySql(h2Database, "metrics", count("id", "total"));
        Assert.assertFalse(sql, sql.contains("count(*)"));
        Assert.assertTrue(sql, sql.contains("count( " + h2Table.getColumnNow("id").getFullName() + " )"));
    }

    private String querySql(RDBDatabaseMetadata database, String tableName, SelectColumn column) {
        return DefaultDatabaseOperator.of(database)
                                      .dml()
                                      .query(tableName)
                                      .select(column)
                                      .getSql()
                                      .getSql();
    }

    private SelectColumn count(String property, String alias) {
        SelectColumn column = SelectColumn.of(property, alias);
        column.setFunction("count");
        return column;
    }

    private void addColumn(RDBTableMetadata table, String name, boolean notNull) {
        RDBColumnMetadata column = table.newColumn();
        column.setName(name);
        column.setType(JdbcDataType.of(JDBCType.VARCHAR, String.class));
        column.setNotNull(notNull);
        table.addColumn(column);
    }
}
