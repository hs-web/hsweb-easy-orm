package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.EmptySqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;

public class ValueByTimeFunctionFragmentBuilderTest {

    @Test
    public void shouldPreferDetectedTimeColumn() {
        RDBTableMetadata table = createTable("metrics");
        RDBColumnMetadata valueColumn = addColumn(table, "value", JDBCType.BIGINT, Long.class);
        RDBColumnMetadata timeColumn = addColumn(table, "event_time", JDBCType.TIMESTAMP, Date.class);

        ValueByTimeFunctionFragmentBuilder builder = new ValueByTimeFunctionFragmentBuilder("first", "第一个值");
        SqlFragments fragments = builder.create(valueColumn.getFullName(), valueColumn, Collections.emptyMap());

        Assert.assertEquals(
            Arrays.asList(
                "first(",
                valueColumn.getFullName(),
                ",",
                timeColumn.getFullName(),
                ")"
            ),
            fragments.getSql()
        );
    }

    @Test
    public void shouldFallbackToNumberColumnWhenNoTimeColumn() {
        RDBTableMetadata table = createTable("metrics");
        RDBColumnMetadata nameColumn = addColumn(table, "name", JDBCType.VARCHAR, String.class);
        RDBColumnMetadata numberColumn = addColumn(table, "sequence", JDBCType.BIGINT, Long.class);

        ValueByTimeFunctionFragmentBuilder builder = new ValueByTimeFunctionFragmentBuilder("last", "最后一值");
        SqlFragments fragments = builder.create(nameColumn.getFullName(), nameColumn, Collections.emptyMap());

        Assert.assertEquals(
            Arrays.asList(
                "last(",
                nameColumn.getFullName(),
                ",",
                numberColumn.getFullName(),
                ")"
            ),
            fragments.getSql()
        );
    }

    @Test
    public void shouldReturnEmptyFragmentsWhenColumnFullNameIsNull() {
        RDBTableMetadata table = createTable("metrics");
        RDBColumnMetadata valueColumn = addColumn(table, "value", JDBCType.DOUBLE, Double.class);
        addColumn(table, "event_time", JDBCType.TIMESTAMP, Date.class);

        ValueByTimeFunctionFragmentBuilder builder = new ValueByTimeFunctionFragmentBuilder("last", "最后一值");

        Assert.assertSame(EmptySqlFragments.INSTANCE, builder.create(null, valueColumn, Collections.emptyMap()));
    }

    @Test
    public void shouldThrowWhenNoTimeOrNumberColumnExists() {
        RDBTableMetadata table = createTable("metrics");
        RDBColumnMetadata textColumn = addColumn(table, "name", JDBCType.VARCHAR, String.class);

        IllegalArgumentException error = Assert.assertThrows(
            IllegalArgumentException.class,
            () -> new ValueByTimeFunctionFragmentBuilder("last", "最后一值")
                .create(textColumn.getFullName(), textColumn, Collections.emptyMap())
        );

        Assert.assertEquals("No time columns", error.getMessage());
    }

    private RDBTableMetadata createTable(String name) {
        RDBSchemaMetadata schema = createSchema();
        RDBTableMetadata table = schema.newTable(name);
        schema.addTable(table);
        return table;
    }

    private RDBSchemaMetadata createSchema() {
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.POSTGRES);
        RDBSchemaMetadata schema = new PostgresqlSchemaMetadata("public");
        database.addSchema(schema);
        database.setCurrentSchema(schema);
        return schema;
    }

    private RDBColumnMetadata addColumn(RDBTableMetadata table, String name, JDBCType jdbcType, Class<?> javaType) {
        RDBColumnMetadata column = table.newColumn();
        column.setName(name);
        column.setType(JdbcDataType.of(jdbcType, javaType));
        table.addColumn(column);
        return column;
    }
}
