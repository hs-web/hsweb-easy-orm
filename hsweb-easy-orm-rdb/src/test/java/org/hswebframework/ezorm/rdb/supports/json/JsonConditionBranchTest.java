package org.hswebframework.ezorm.rdb.supports.json;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.DataTypeBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlJsonTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlJsonTermFragmentBuilder;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;
import java.sql.SQLType;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class JsonConditionBranchTest {

    @Test
    public void testJsonValueConditionParsingVariants() {
        Assert.assertEquals("name", JsonValueCondition.of(Term.of("x", JsonTermType.value, Map.of("name", "name", "value", 1))).getPath());
        Assert.assertEquals(TermType.like, JsonValueCondition.of(Term.of("x", JsonTermType.value, Map.of("path", "name", "operator", TermType.like, "value", "Jet%"))).getTermType());
        Assert.assertEquals("name", JsonValueCondition.of(Term.of("x", JsonTermType.value, List.of("name", 1, TermType.gt))).getPath());
        Assert.assertEquals(TermType.gt, JsonValueCondition.of(Term.of("x", JsonTermType.value, new Object[]{"age", 18, TermType.gt})).getTermType());
        Assert.assertEquals(TermType.eq, JsonValueCondition.of(Term.of("x", JsonTermType.value, Collections.singletonMap("name", "JetLinks"))).getTermType());
        Assert.assertEquals(TermType.nin, JsonValueCondition.of(Term.of("x", JsonTermType.value, new Object[]{"state", "a,b", TermType.nin})).getTermType());
        Assert.assertEquals("state", JsonValueCondition.of(Term.of("x", JsonTermType.value, Arrays.asList("state", "ok", TermType.eq))).getPath());
    }

    @Test
    public void testAbstractJsonTermFragmentBranches() {
        RDBColumnMetadata jsonColumn = jsonColumn(new MysqlSchemaMetadata("test"));
        TestBuilder builder = new TestBuilder();
        ExistsBuilder existsBuilder = new ExistsBuilder();

        Assert.assertEquals("json_contains_path( data ,'one', ? )", builder.exists("data", jsonColumn, "name").toRequest().getSql());
        Assert.assertEquals("not json_contains_path( data ,'one', ? )", existsBuilder.createFragments("data", jsonColumn, Term.of("data", JsonTermType.notExists, "name")).toRequest().getSql());
        Assert.assertEquals("json_contains( data , ? )", builder.createFragments("data", jsonColumn, Term.of("data", JsonTermType.contained, Map.of("name", "JetLinks"))).toRequest().getSql());
        Assert.assertEquals("( json_unquote(json_extract(data,?)) = ? and cast(json_unquote(json_extract(data,?)) as decimal(65,30)) = ? )",
                            builder.containsByValue("data", jsonColumn, new LinkedHashMap<String, Object>() {{ put("name", "JetLinks"); put("profile.age", 18); }}).toRequest().getSql());
        Assert.assertTrue(new PgValueBuilder(false).value("data", jsonColumn, JsonValueCondition.of("state", TermType.eq, "enabled")).toRequest().getSql().contains("#>> array[?]) = ?"));
        Assert.assertTrue(new PgValueBuilder(false).value("data", jsonColumn, JsonValueCondition.of("score", TermType.gt, 18)).toRequest().getSql().contains("cast((data::jsonb #>> array[?]) as numeric) > ?"));
        Assert.assertTrue(new PgValueBuilder(false).value("data", jsonColumn, JsonValueCondition.of("score", TermType.nin, "1,2")).toRequest().getSql().contains("not in("));
        Assert.assertTrue(new PgValueBuilder(true).value("data", jsonColumn, JsonValueCondition.of("state", TermType.in, Arrays.asList("a", "b"))).toRequest().getSql().contains("in( ?,? )"));
        Assert.assertEquals("(data::jsonb #>> array[?]) is null", new PgValueBuilder(false).value("data", jsonColumn, JsonValueCondition.of("name", TermType.isnull, null)).toRequest().getSql());
        Assert.assertEquals("(data::jsonb #>> array[?]) is not null", new PgValueBuilder(false).value("data", jsonColumn, JsonValueCondition.of("name", TermType.notnull, null)).toRequest().getSql());
        Assert.assertTrue(builder.containsByValue("data", jsonColumn, Collections.emptyMap()).isEmpty());
    }

    @Test
    public void testUnsupportedJsonValueTermThrows() {
        RDBColumnMetadata jsonColumn = jsonColumn(new MysqlSchemaMetadata("test"));
        try {
            new PgValueBuilder(false).value("data", jsonColumn, JsonValueCondition.of("name", "unsupported", "JetLinks")).toRequest();
            Assert.fail("expected exception");
        } catch (UnsupportedOperationException expected) {
            Assert.assertTrue(expected.getMessage().contains("unsupported"));
        }
    }

    private static RDBColumnMetadata jsonColumn(RDBSchemaMetadata schema) {
        RDBTableMetadata table = schema.newTable("test_json_condition");
        RDBColumnMetadata column = table.newColumn();
        column.setName("data");
        column.setType(JsonType.INSTANCE);
        table.addColumn(column);
        return column;
    }

    private static class TestBuilder extends MysqlJsonTermFragmentBuilder {
        private TestBuilder() {
            super("json_contains_by_value", "json contains", Operation.contains, false);
        }

        private SqlFragments exists(String columnFullName, RDBColumnMetadata column, Object value) {
            return createExistsFragments(columnFullName, column, value, false);
        }

        private SqlFragments containsByValue(String columnFullName, RDBColumnMetadata column, Object value) {
            return createContainsByValueFragments(columnFullName, column, value, false);
        }
    }

    private static class ExistsBuilder extends MysqlJsonTermFragmentBuilder {
        private ExistsBuilder() {
            super("json_exists", "json exists", Operation.exists, true);
        }
    }

    private static class PgValueBuilder extends PostgresqlJsonTermFragmentBuilder {
        private PgValueBuilder(boolean not) {
            super("json_value", "json value", Operation.value, not);
        }

        private SqlFragments value(String columnFullName, RDBColumnMetadata column, JsonValueCondition condition) {
            return createValueFragments(columnFullName, column, condition, false);
        }
    }
}
