package org.hswebframework.ezorm.rdb.supports.postgres;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.annotation.ColumnType;
import org.hswebframework.ezorm.rdb.mapping.defaults.record.Record;
import org.hswebframework.ezorm.rdb.mapping.jpa.SimpleEntityPropertyDescriptor;
import org.hswebframework.ezorm.rdb.mapping.parser.DefaultDataTypeResolver;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.DefaultValueCodecFactory;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.junit.Test;

import java.beans.PropertyDescriptor;
import java.math.BigDecimal;
import java.sql.JDBCType;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.*;

public class UnconstrainedNumericTest {
    @Test
    public void precisionDeclarationIsExplicitAndDialectOwned() {
        for (JDBCType type : List.of(JDBCType.NUMERIC, JDBCType.DECIMAL)) {
            var column = new PostgresqlSchemaMetadata("public").newTable("numbers").newColumn();
            column.setJdbcType(type, BigDecimal.class);
            assertEquals(type == JDBCType.NUMERIC ? "numeric(32,0)" : "decimal(32,0)", Dialect.POSTGRES.buildColumnDataType(column));
            column.setPrecision(18); column.setScale(6);
            assertEquals(type == JDBCType.NUMERIC ? "numeric(18,6)" : "decimal(18,6)", Dialect.POSTGRES.buildColumnDataType(column));
            column.setUnconstrainedNumeric(true);
            assertEquals(type == JDBCType.NUMERIC ? "numeric" : "decimal", Dialect.POSTGRES.buildColumnDataType(column));
            assertTrue(column.clone().isUnconstrainedNumeric());
            var fixed = column.clone();
            fixed.setName("amount"); column.setName("amount");
            fixed.setUnconstrainedNumeric(false);
            assertTrue(fixed.ddlModifiable(column));
            fixed.setPrecision(40); fixed.setScale(12);
            assertFalse(column.ddlModifiable(fixed));
            for (Dialect unsupported : List.of(Dialect.MYSQL, Dialect.H2, Dialect.ORACLE)) {
                assertThrows(UnsupportedOperationException.class, () -> unsupported.buildColumnDataType(column));
            }
        }
        var column = new RDBColumnMetadata();
        column.setJdbcType(JDBCType.DOUBLE, Double.class); column.setUnconstrainedNumeric(true);
        assertThrows(IllegalArgumentException.class, () -> Dialect.POSTGRES.buildColumnDataType(column));
    }

    @Test
    public void annotationUsesExistingBigDecimalCodecAndOverridesTypeBuilderDefaults() throws Exception {
        var column = new PostgresqlSchemaMetadata("public").newTable("numbers").newColumn();
        var descriptor = SimpleEntityPropertyDescriptor.of(new PropertyDescriptor("amount", NumericEntity.class), column);
        column.setType(DefaultDataTypeResolver.INSTANCE.resolve(descriptor));
        assertTrue(column.isUnconstrainedNumeric());
        assertEquals("numeric", column.getDataType());
        var codec = DefaultValueCodecFactory.COMMONS.createValueCodec(column).orElseThrow();
        var exact = new BigDecimal("12345678901234567890.1234567890123456789");
        assertEquals(exact, codec.decode(codec.encode(exact)));
    }

    @Test
    public void introspectionDistinguishesUnconstrainedAndFixedScaleIncludingZero() {
        var schema = new PostgresqlSchemaMetadata("public");
        var parser = new PostgresqlTableMetadataParser(schema) {
            void read(RDBColumnMetadata column, Record record) { applyColumnInfo(column, record); }
        };
        var column = schema.newTable("numbers").newColumn();
        var record = Record.newRecord().putValue("name", "amount").putValue("data_type", "numeric");
        parser.read(column, record);
        assertTrue(column.isUnconstrainedNumeric()); assertEquals("numeric", column.getDataType());
        record.putValue("data_precision", 40).putValue("data_scale", 0);
        parser.read(column, record);
        assertFalse(column.isUnconstrainedNumeric()); assertEquals("numeric(40,0)", column.getDataType());
    }

    @Test
    public void postgresRoundTripAggregationAndSchemaReapplicationKeepPrecision() throws Exception {
        var provider = new PostgresqlConnectionProvider();
        var executor = new TestSyncSqlExecutor(provider);
        var schema = new PostgresqlSchemaMetadata("public");
        var database = new RDBDatabaseMetadata(Dialect.POSTGRES);
        database.addSchema(schema); database.setCurrentSchema(schema); database.addFeature(executor);
        var operator = DefaultDatabaseOperator.of(database);
        String name = "orm_numeric_" + UUID.randomUUID().toString().replace("-", "");
        var table = schema.newTable(name);
        var column = table.newColumn(); column.setName("amount");
        column.setJdbcType(JDBCType.NUMERIC, BigDecimal.class);
        column.setPrecision(18); column.setScale(6);
        table.addColumn(column);
        try {
            operator.ddl().createOrAlter(table).commit().sync();
            var unconstrained = table.clone();
            unconstrained.getColumnNow("amount").setUnconstrainedNumeric(true);
            operator.ddl().createOrAlter(unconstrained).commit().sync();
            var exact = new BigDecimal("123456789012345678901234567890.1234567890123456789");
            try (var connection = provider.getConnection()) {
                try (var insert = connection.prepareStatement("insert into " + name + " (amount) values (?)")) {
                    for (BigDecimal value : List.of(exact, exact.add(new BigDecimal("2")))) {
                        insert.setObject(1, column.encode(value)); insert.executeUpdate();
                    }
                }
                try (var query = connection.createStatement(); var result = query.executeQuery("select min(amount),sum(amount),avg(amount) from " + name)) {
                    assertTrue(result.next());
                    assertEquals(0, exact.compareTo(result.getBigDecimal(1)));
                    assertEquals(0, exact.multiply(new BigDecimal("2")).add(new BigDecimal("2")).compareTo(result.getBigDecimal(2)));
                    assertEquals(0, exact.add(BigDecimal.ONE).compareTo(result.getBigDecimal(3)));
                }
                var restored = new PostgresqlTableMetadataParser(schema).parseByName(name).orElseThrow();
                var reflected = restored.getColumnNow("amount");
                assertTrue(reflected.isUnconstrainedNumeric()); assertEquals("numeric", reflected.getDataType());
                assertEquals(BigDecimal.class, reflected.getJavaType());
                assertEquals(exact, reflected.decode(exact));
                operator.ddl().createOrAlter(restored).commit().sync();
                var constrained = restored.clone();
                constrained.getColumnNow("amount").setUnconstrainedNumeric(false);
                constrained.getColumnNow("amount").setPrecision(60);
                constrained.getColumnNow("amount").setScale(25);
                operator.ddl().createOrAlter(constrained).commit().sync();
                try (var query = connection.prepareStatement("select numeric_precision,numeric_scale from information_schema.columns where table_schema='public' and table_name=? and column_name='amount'")) {
                    query.setString(1, name);
                    try (var result = query.executeQuery()) {
                        assertTrue(result.next()); assertNull(result.getObject(1)); assertNull(result.getObject(2));
                    }
                }
            }
        } finally {
            try (var connection = provider.getConnection(); var statement = connection.createStatement()) {
                statement.execute("drop table if exists " + name);
            }
        }
    }

    @Getter
    @Setter
    public static class NumericEntity {
        @ColumnType(jdbcType = JDBCType.NUMERIC, unconstrainedNumeric = true)
        private BigDecimal amount;
    }
}
