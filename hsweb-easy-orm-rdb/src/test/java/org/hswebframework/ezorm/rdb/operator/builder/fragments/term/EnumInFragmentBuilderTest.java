package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.mssql.SqlServerEnumInFragmentBuilder;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlEnumInFragmentBuilder;
import org.hswebframework.ezorm.rdb.supports.oracle.OracleEnumInFragmentBuilder;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlEnumInFragmentBuilder;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;

public class EnumInFragmentBuilderTest {

    private enum State {
        enabled, disabled, offline
    }

    @Test
    public void testDialectSpecificBuilderSelection() {
        Assert.assertSame(MysqlEnumInFragmentBuilder.in, EnumInFragmentBuilder.of(Dialect.MYSQL));
        Assert.assertSame(MysqlEnumInFragmentBuilder.notIn, EnumInFragmentBuilder.ofNot(Dialect.MYSQL));
        Assert.assertSame(PostgresqlEnumInFragmentBuilder.in, EnumInFragmentBuilder.of(Dialect.POSTGRES));
        Assert.assertSame(PostgresqlEnumInFragmentBuilder.notIn, EnumInFragmentBuilder.ofNot(Dialect.POSTGRES));
        Assert.assertSame(SqlServerEnumInFragmentBuilder.in, EnumInFragmentBuilder.of(Dialect.MSSQL));
        Assert.assertSame(SqlServerEnumInFragmentBuilder.notIn, EnumInFragmentBuilder.ofNot(Dialect.MSSQL));
        Assert.assertSame(OracleEnumInFragmentBuilder.in, EnumInFragmentBuilder.of(Dialect.ORACLE));
        Assert.assertSame(OracleEnumInFragmentBuilder.notIn, EnumInFragmentBuilder.ofNot(Dialect.ORACLE));
        Assert.assertNotSame(EnumInFragmentBuilder.of(Dialect.H2), EnumInFragmentBuilder.of(Dialect.H2));
    }

    @Test
    public void testAnyAndAllMaskConditions() {
        RDBColumnMetadata column = column();

        Term any = Term.of("state", "in", new HashSet<>(Arrays.asList(State.enabled, State.offline, 8)));
        any.getOptions().add("any");
        SqlRequest anyRequest = EnumInFragmentBuilder.of(Dialect.H2)
            .createFragments("t.STATE", column, any)
            .toRequest();
        Assert.assertEquals("t.STATE & 13 != 0", anyRequest.getSql());

        Term all = Term.of("state", "in", new Object[]{1L, 4L});
        SqlRequest allRequest = EnumInFragmentBuilder.of(Dialect.H2)
            .createFragments("t.STATE", column, all)
            .toRequest();
        Assert.assertEquals("t.STATE & 5 = t.STATE", allRequest.getSql());

        Term notAny = Term.of("state", "nin", Collections.singletonList(State.disabled));
        notAny.getOptions().add("any");
        SqlRequest notAnyRequest = EnumInFragmentBuilder.ofNot(Dialect.H2)
            .createFragments("t.STATE", column, notAny)
            .toRequest();
        Assert.assertEquals("t.STATE & 2 = 0", notAnyRequest.getSql());

        Term notAll = Term.of("state", "nin", new Object[]{State.enabled, State.disabled});
        SqlRequest notAllRequest = EnumInFragmentBuilder.ofNot(Dialect.H2)
            .createFragments("t.STATE", column, notAll)
            .toRequest();
        Assert.assertEquals("t.STATE & 3 != t.STATE", notAllRequest.getSql());
    }

    @Test
    public void testOracleUsesParameterForBitMask() {
        SqlRequest request = EnumInFragmentBuilder.of(Dialect.ORACLE)
            .createFragments("T.STATE", column(), Term.of("state", "in", State.offline))
            .toRequest();

        Assert.assertEquals("BITAND( T.STATE , ? ) = T.STATE".replace("? ", "?"), request.getSql());
        Assert.assertArrayEquals(new Object[]{4L}, request.getParameters());
    }

    private RDBColumnMetadata column() {
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.H2);
        RDBSchemaMetadata schema = new RDBSchemaMetadata("PUBLIC");
        database.addSchema(schema);
        database.setCurrentSchema(schema);
        RDBTableMetadata table = schema.newTable("test_enum");
        RDBColumnMetadata column = table.newColumn();
        column.setName("state");
        column.setType(JdbcDataType.of(JDBCType.NUMERIC, Long.class));
        table.addColumn(column);
        return column;
    }
}
