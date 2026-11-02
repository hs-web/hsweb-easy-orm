package org.hswebframework.ezorm.rdb.supports.json;

import org.hswebframework.ezorm.core.param.TermType;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.junit.Assert;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class AbstractJsonTermFragmentBuilderCoverageTest {

    @Test
    public void testJsonValueOperatorsAndReverseOperators() {
        RDBColumnMetadata column = column();
        ValueBuilder normal = new ValueBuilder(false);
        ValueBuilder reversed = new ValueBuilder(true);

        assertSql(normal, column, TermType.eq, "enabled", "str(data,state) = ?");
        assertSql(normal, column, "is", "enabled", "str(data,state) = ?");
        assertSql(normal, column, TermType.not, "disabled", "str(data,state) != ?");
        assertSql(normal, column, TermType.gt, 18, "num(data,state) > ?");
        assertSql(normal, column, TermType.gte, 18, "num(data,state) >= ?");
        assertSql(normal, column, TermType.lt, 18, "num(data,state) < ?");
        assertSql(normal, column, TermType.lte, 18, "num(data,state) <= ?");
        assertSql(normal, column, TermType.like, "Jet%", "str(data,state) like ?");
        assertSql(normal, column, TermType.nlike, "Jet%", "str(data,state) not like ?");

        assertSql(reversed, column, TermType.eq, "enabled", "str(data,state) != ?");
        assertSql(reversed, column, TermType.not, "disabled", "str(data,state) = ?");
        assertSql(reversed, column, TermType.gt, 18, "num(data,state) <= ?");
        assertSql(reversed, column, TermType.gte, 18, "num(data,state) < ?");
        assertSql(reversed, column, TermType.lt, 18, "num(data,state) >= ?");
        assertSql(reversed, column, TermType.lte, 18, "num(data,state) > ?");
        assertSql(reversed, column, TermType.like, "Jet%", "str(data,state) not like ?");
        assertSql(reversed, column, TermType.nlike, "Jet%", "str(data,state) like ?");
    }

    @Test
    public void testInNinNullNativeAndNumberDetectionBranches() {
        RDBColumnMetadata column = column();
        ValueBuilder normal = new ValueBuilder(false);
        ValueBuilder reversed = new ValueBuilder(true);

        Assert.assertTrue(normal.value("data", column, JsonValueCondition.of("state", TermType.in, null)).isEmpty());
        Assert.assertEquals("str(data,state) in( ?,? )", normal.value("data", column, JsonValueCondition.of("state", TermType.in, "a,b")).toRequest().getSql());
        Assert.assertEquals("str(data,state) not in( ?,? )", normal.value("data", column, JsonValueCondition.of("state", TermType.nin, new String[]{"a", "b"})).toRequest().getSql());
        Assert.assertEquals("num(data,score) in( ?,? )", normal.value("data", column, JsonValueCondition.of("score", TermType.in, Arrays.asList(1, 2))).toRequest().getSql());
        Assert.assertEquals("num(data,score) in( ?,? )", normal.value("data", column, JsonValueCondition.of("score", TermType.in, new Number[]{1, 2})).toRequest().getSql());
        Assert.assertEquals("str(data,state) not in( ?,? )", reversed.value("data", column, JsonValueCondition.of("state", TermType.in, List.of("a", "b"))).toRequest().getSql());
        Assert.assertEquals("str(data,state) in( ?,? )", reversed.value("data", column, JsonValueCondition.of("state", TermType.nin, List.of("a", "b"))).toRequest().getSql());

        SqlRequest nativeValue = normal.value("data", column, JsonValueCondition.of("state", TermType.eq, NativeSql.of("upper(?)", "ok"))).toRequest();
        Assert.assertEquals("str(data,state) = upper(?)", nativeValue.getSql());
        Assert.assertArrayEquals(new Object[]{"ok"}, nativeValue.getParameters());

        Assert.assertEquals("str(data,state) is null", normal.value("data", column, JsonValueCondition.of("state", TermType.isnull, null)).toRequest().getSql());
        Assert.assertEquals("str(data,state) is not null", normal.value("data", column, JsonValueCondition.of("state", TermType.notnull, null)).toRequest().getSql());
        Assert.assertEquals("str(data,state) is not null", reversed.value("data", column, JsonValueCondition.of("state", TermType.isnull, null)).toRequest().getSql());
        Assert.assertEquals("str(data,state) is null", reversed.value("data", column, JsonValueCondition.of("state", TermType.notnull, null)).toRequest().getSql());
    }

    @Test
    public void testContainsByValueFlattenAndDefaultContainedUnsupported() {
        RDBColumnMetadata column = column();
        ValueBuilder builder = new ValueBuilder(false);
        Map<String, Object> nested = new LinkedHashMap<>();
        nested.put(null, "ignored");
        nested.put("name", "JetLinks");
        nested.put("profile", Collections.singletonMap("age", 18));

        SqlRequest request = builder.containsByValue("data", column, nested).toRequest();
        Assert.assertEquals("( str(data,name) = ? and num(data,profile.age) = ? )", request.getSql());
        Assert.assertArrayEquals(new Object[]{"JetLinks", 18}, request.getParameters());
        Assert.assertTrue(builder.containsByValue("data", column, "not-map").isEmpty());
        Assert.assertEquals("not", builder.nullable(true));
        Assert.assertEquals("", builder.nullable(false));

        try {
            new DefaultContainedBuilder().contained("data", column, Collections.emptyMap()).toRequest();
            Assert.fail("default contained branch should fail fast");
        } catch (UnsupportedOperationException expected) {
            Assert.assertTrue(expected.getMessage().contains("does not support"));
        }
    }

    private void assertSql(ValueBuilder builder, RDBColumnMetadata column, String termType, Object value, String sql) {
        Assert.assertEquals(sql, builder.value("data", column, JsonValueCondition.of("state", termType, value)).toRequest().getSql());
    }

    private RDBColumnMetadata column() {
        RDBTableMetadata table = new H2SchemaMetadata("PUBLIC").newTable("json_term_branch");
        RDBColumnMetadata column = table.newColumn();
        column.setName("data");
        column.setType(JsonType.INSTANCE);
        table.addColumn(column);
        return column;
    }

    private static class ValueBuilder extends AbstractJsonTermFragmentBuilder {
        private final boolean not;

        private ValueBuilder(boolean not) {
            super("json_value_test", "json value test", Operation.value, not);
            this.not = not;
        }

        private SqlFragments value(String columnFullName, RDBColumnMetadata column, JsonValueCondition condition) {
            return createValueFragments(columnFullName, column, condition, not);
        }

        private SqlFragments containsByValue(String columnFullName, RDBColumnMetadata column, Object value) {
            return createContainsByValueFragments(columnFullName, column, value, not);
        }

        private String nullable(boolean not) {
            return nullableNot(not);
        }

        @Override
        protected SqlFragments createExistsFragments(String columnFullName, RDBColumnMetadata column, Object path, boolean not) {
            return PrepareSqlFragments.of("exists");
        }

        @Override
        protected SqlFragments createContainsFragments(String columnFullName, RDBColumnMetadata column, Object value, boolean not) {
            return PrepareSqlFragments.of("contains");
        }

        @Override
        protected JsonScalarExpression createScalarExpression(String columnFullName, RDBColumnMetadata column, JsonValueCondition condition, boolean number) {
            return JsonScalarExpression.of((number ? "num" : "str") + "(" + columnFullName + "," + condition.getPath() + ")", Collections.emptyList());
        }
    }

    private static class DefaultContainedBuilder extends ValueBuilder {
        private DefaultContainedBuilder() {
            super(false);
        }

        private SqlFragments contained(String columnFullName, RDBColumnMetadata column, Object value) {
            return createContainedFragments(columnFullName, column, value, false);
        }
    }
}
