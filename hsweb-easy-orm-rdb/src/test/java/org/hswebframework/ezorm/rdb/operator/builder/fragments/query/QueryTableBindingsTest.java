package org.hswebframework.ezorm.rdb.operator.builder.fragments.query;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.MetadataHelper;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.dml.Join;
import org.hswebframework.ezorm.rdb.operator.dml.JoinType;
import org.hswebframework.ezorm.rdb.operator.dml.QueryOperator;
import org.hswebframework.ezorm.rdb.operator.dml.query.ExecutableQueryOperator;
import org.hswebframework.ezorm.rdb.operator.dml.query.QueryOperatorParameter;
import org.hswebframework.ezorm.rdb.operator.dml.query.QueryTableBindings;
import org.hswebframework.ezorm.rdb.operator.dml.query.SelectColumn;
import org.hswebframework.ezorm.rdb.operator.dml.query.SortOrder;
import org.junit.Test;

import java.sql.JDBCType;
import java.util.Collections;
import java.util.Map;

import static org.junit.Assert.*;

public class QueryTableBindingsTest {
    @Test
    public void detachedSnapshotResolvesJoinProjectionFilterAndSortWithoutMutatingSchema() {
        RDBSchemaMetadata schema = MetadataHelper.createMockSchema();
        RDBTableMetadata main = schema.getTable("test").orElseThrow(IllegalStateException::new);
        RDBTableMetadata remote = schema.newTable("remote");
        column(remote, "owner_id", "ownerId");
        column(remote, "old_value", "value");
        QueryOperator query = new ExecutableQueryOperator(main);
        Join join = new Join();
        join.setTarget("remote");
        join.setAlias("ext");
        join.setType(JoinType.left);
        join.setTerms(Collections.singletonList(Term.of("ownerId", "eq", (NativeSql) () -> "test.id")));
        query.bindTables(new QueryTableBindings(Collections.singletonMap("remote", remote)))
            .join(join).select(SelectColumn.of("ext.value", "extensionValue"))
            .where(Term.of("ext.value", "eq", "expected"))
            .orderBy(SortOrder.desc("ext.value"));
        String sql = query.getSql().getSql();
        assertTrue(sql, sql.contains("remote"));
        assertTrue(sql, sql.toLowerCase().contains("old_value"));
        assertTrue(sql, sql.toLowerCase().contains("order by"));
        assertTrue(sql, sql.contains("extensionValue"));
        assertFalse(schema.getTable("remote", false).isPresent());
        // A newer mapping cannot change the already retained query's projection or predicates.
        RDBTableMetadata newer = schema.newTable("remote");
        column(newer, "new_value", "value");
        new QueryTableBindings(Collections.singletonMap("remote", newer));
        assertEquals(sql, query.clone().getSql().getSql());
    }

    @Test
    public void untrustedContextCannotInstallMappingsAndDefaultLookupIsUnchanged() {
        RDBSchemaMetadata schema = MetadataHelper.createMockSchema();
        RDBTableMetadata main = schema.getTable("test").orElseThrow(IllegalStateException::new);
        QueryOperatorParameter parameter = new QueryOperatorParameter();
        assertSame(main, parameter.findTable(main, "test").orElse(null));
        parameter.setContext(Collections.singletonMap("easyorm.query.table-bindings",
            Collections.singletonMap("test", Collections.singletonMap("name", "other"))));
        assertSame(main, parameter.findTable(main, "test").orElse(null));
    }

    @Test
    public void explicitBindingsSurviveContextChangesAndCloneWithoutInstallingJoins() {
        RDBSchemaMetadata schema = MetadataHelper.createMockSchema();
        RDBTableMetadata main = schema.getTable("test").orElseThrow(IllegalStateException::new);
        RDBTableMetadata remote = schema.newTable("remote");
        QueryTableBindings bindings = new QueryTableBindings(Collections.singletonMap("remote", remote));
        QueryOperatorParameter parameter = new QueryOperatorParameter();
        Join explicit = new Join();
        explicit.setTarget("ordinary");
        parameter.getJoins().add(explicit);
        parameter.setTableBindings(bindings);
        assertSame(remote, parameter.findTable(main, "remote").orElse(null));
        parameter.setContext(Collections.singletonMap("easyorm.query.table-bindings", bindings));
        parameter.setContext(null);
        assertSame(remote, parameter.findTable(main, "remote").orElse(null));
        assertEquals(1, parameter.getJoins().size());
        QueryOperatorParameter copy = parameter.clone();
        assertSame(remote, copy.findTable(main, "remote").orElse(null));
        copy.setTableBindings(null);
        assertFalse(copy.findTable(main, "remote").isPresent());
        assertEquals(1, copy.getJoins().size());
        assertSame(remote, parameter.findTable(main, "remote").orElse(null));
        assertEquals(1, parameter.getJoins().size());
        parameter.setTableBindings(null);
        parameter.setContext(Collections.singletonMap("easyorm.query.table-bindings", bindings));
        assertFalse(parameter.findTable(main, "remote").isPresent());
    }

    private static void column(RDBTableMetadata table, String name, String alias) {
        RDBColumnMetadata column = table.newColumn();
        column.setName(name);
        column.setAlias(alias);
        column.setJdbcType(JDBCType.VARCHAR, String.class);
        table.addColumn(column);
    }
}
