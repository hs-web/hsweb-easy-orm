package org.hswebframework.ezorm.rdb.supports.json;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.DataTypeBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.TermFragmentBuilder;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlJsonTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.postgres.JsonbType;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlJsonTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlSchemaMetadata;
import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;
import java.sql.SQLType;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

public class JsonBranchBehaviorTest {

    @Test
    public void testJsonFeatureRegistrationOnlyForJsonColumns() {
        RDBColumnMetadata nullColumn = new RDBColumnMetadata();
        AbstractJsonTermFragmentBuilder.addJsonFeatures(nullColumn, MysqlJsonTermFragmentBuilder.exists);
        Assert.assertFalse(nullColumn.findFeature(TermFragmentBuilder.createFeatureId(JsonTermType.exists)).isPresent());

        RDBColumnMetadata varcharColumn = new RDBColumnMetadata();
        varcharColumn.setType(new SimpleDataType("varchar"));
        AbstractJsonTermFragmentBuilder.addJsonFeatures(varcharColumn, MysqlJsonTermFragmentBuilder.exists);
        Assert.assertFalse(varcharColumn.findFeature(TermFragmentBuilder.createFeatureId(JsonTermType.exists)).isPresent());

        RDBColumnMetadata jsonById = new RDBColumnMetadata();
        jsonById.setType(new SimpleDataType("json"));
        AbstractJsonTermFragmentBuilder.addJsonFeatures(jsonById, MysqlJsonTermFragmentBuilder.exists);
        Assert.assertTrue(jsonById.findFeature(TermFragmentBuilder.createFeatureId(JsonTermType.exists)).isPresent());
    }

    @Test
    public void testJsonPathNormalizeAndSegmentsCoverRepositoryInputs() {
        Assert.assertEquals("$", JsonPathUtils.normalize(null));
        Assert.assertEquals("$", JsonPathUtils.normalize("  "));
        Assert.assertEquals("$", JsonPathUtils.normalize("$"));
        Assert.assertEquals("$.name", JsonPathUtils.normalize(".name"));
        Assert.assertEquals("$[0]", JsonPathUtils.normalize("[0]"));
        Assert.assertEquals("$.name", JsonPathUtils.normalize("name"));

        Assert.assertEquals(Collections.emptyList(), JsonPathUtils.segments(null));
        Assert.assertEquals(Arrays.asList("detail", "name"), JsonPathUtils.segments("$.detail['name']"));
        Assert.assertEquals(Arrays.asList("detail", "name"), JsonPathUtils.segments("detail[\"name\"]"));
        Assert.assertEquals(Arrays.asList("detail", "[broken", "name"), JsonPathUtils.segments("detail.[broken.name"));
        Assert.assertEquals(Arrays.asList("items", "0", "name"), JsonPathUtils.segments("items[0].name"));
    }

    @Test
    public void testPostgresqlJsonBranchesForNegativeNativeAndValueOperators() {
        RDBColumnMetadata column = jsonColumn(new PostgresqlSchemaMetadata("public"), JsonbType.INSTANCE);

        SqlRequest rootExists = create(column, Term.of("data", JsonTermType.exists, null)).toRequest();
        Assert.assertEquals("data is not null", rootExists.getSql());

        SqlRequest rootNotExists = create(column, Term.of("data", JsonTermType.notExists, "$" )).toRequest();
        Assert.assertEquals("data is null", rootNotExists.getSql());

        SqlRequest notContainsNative = create(column, Term.of("data", JsonTermType.notContains, NativeSql.of("?::jsonb", "{\"name\":\"bad\"}"))).toRequest();
        Assert.assertEquals("not ( data ::jsonb @> ?::jsonb )", notContainsNative.getSql());
        Assert.assertArrayEquals(new Object[]{"{\"name\":\"bad\"}"}, notContainsNative.getParameters());

        SqlRequest contained = create(column, Term.of("data", JsonTermType.contained, Collections.singletonMap("name", "JetLinks"))).toRequest();
        Assert.assertEquals("data ::jsonb <@ ?::jsonb", contained.getSql());
        Assert.assertEquals("{\"name\":\"JetLinks\"}", contained.getParameters()[0]);

        SqlRequest notContained = create(column, Term.of("data", JsonTermType.notContained, NativeSql.of("?::jsonb", "{}"))).toRequest();
        Assert.assertEquals("not ( data ::jsonb <@ ?::jsonb )", notContained.getSql());
        Assert.assertArrayEquals(new Object[]{"{}"}, notContained.getParameters());

        Assert.assertEquals(
            "(data::jsonb #>> array[?]) != ?",
            new FallbackValueBuilder(true).value("data", column, JsonValueCondition.of("name", TermType.eq, "disabled")).toRequest().getSql()
        );
        Assert.assertEquals(
            "(data::jsonb #>> array[?]) not like ?",
            new FallbackValueBuilder(true).value("data", column, JsonValueCondition.of("name", TermType.like, "Jet%")).toRequest().getSql()
        );
        Assert.assertEquals(
            "cast((data::jsonb #>> array[?]) as numeric) <= ?",
            new FallbackValueBuilder(true).value("data", column, JsonValueCondition.of("age", TermType.gt, 18)).toRequest().getSql()
        );
        Assert.assertEquals(
            "cast((data::jsonb #>> array[?]) as numeric) not in( ?,?,? )",
            new FallbackValueBuilder(true).value("data", column, JsonValueCondition.of("score", TermType.in, new Number[]{1, 2, 3})).toRequest().getSql()
        );
        Assert.assertEquals(
            "(data::jsonb #>> array[?]) in( ?,? )",
            new FallbackValueBuilder(true).value("data", column, JsonValueCondition.of("state", TermType.nin, "disabled,offline")).toRequest().getSql()
        );
        Assert.assertTrue(
            create(column, Term.of("data", JsonTermType.value, JsonValueCondition.of("state", TermType.in, Collections.emptyList()))).isEmpty()
        );
    }

    @Test
    public void testMysqlJsonContainedAndContainsByValueFallbackBranches() {
        RDBColumnMetadata column = jsonColumn(new MysqlSchemaMetadata("test"), JsonType.INSTANCE);

        SqlRequest notExists = create(column, Term.of("data", JsonTermType.notExists, "name")).toRequest();
        Assert.assertEquals("not json_contains_path( data ,'one', ? )", notExists.getSql());
        Assert.assertArrayEquals(new Object[]{"$.name"}, notExists.getParameters());

        SqlRequest notContainsNative = create(column, Term.of("data", JsonTermType.notContains, NativeSql.of("json_object(?,?)", "name", "bad"))).toRequest();
        Assert.assertEquals("not json_contains( data , json_object(?,?) )", notContainsNative.getSql());
        Assert.assertArrayEquals(new Object[]{"name", "bad"}, notContainsNative.getParameters());

        SqlRequest contained = create(column, Term.of("data", JsonTermType.contained, Collections.singletonMap("name", "JetLinks"))).toRequest();
        Assert.assertEquals("json_contains( ? , data )", contained.getSql());
        Assert.assertEquals("{\"name\":\"JetLinks\"}", contained.getParameters()[0]);

        SqlRequest notContained = create(column, Term.of("data", JsonTermType.notContained, NativeSql.of("json_object(?,?)", "name", "JetLinks"))).toRequest();
        Assert.assertEquals("not json_contains( json_object(?,?) , data )", notContained.getSql());
        Assert.assertArrayEquals(new Object[]{"name", "JetLinks"}, notContained.getParameters());

        Map<String, Object> nested = new LinkedHashMap<>();
        nested.put(null, "ignored");
        nested.put("name", "JetLinks");
        nested.put("profile", Collections.singletonMap("age", 18));
        SqlRequest fallback = new FallbackContainsBuilder(false).containsByValue("data", column, nested).toRequest();
        Assert.assertEquals("( json_unquote(json_extract(data,?)) = ? and cast(json_unquote(json_extract(data,?)) as decimal(65,30)) = ? )", fallback.getSql());
        Assert.assertArrayEquals(new Object[]{"$.name", "JetLinks", "$.profile.age", 18}, fallback.getParameters());

        Assert.assertTrue(new FallbackContainsBuilder(false).containsByValue("data", column, "not-map").isEmpty());
        Assert.assertEquals("not ( json_unquote(json_extract(data,?)) = ? )",
                            new FallbackContainsBuilder(true).containsByValue("data", column, Collections.singletonMap("name", "bad")).toRequest().getSql());
    }

    @Test(expected = UnsupportedOperationException.class)
    public void testUnsupportedJsonValueTermTypeFailsFast() {
        RDBColumnMetadata column = jsonColumn(new MysqlSchemaMetadata("test"), JsonType.INSTANCE);
        create(column, Term.of("data", JsonTermType.value, JsonValueCondition.of("name", "unsupported", "JetLinks"))).toRequest();
    }

    private SqlFragments create(RDBColumnMetadata column, Term term) {
        TermFragmentBuilder builder = column
            .findFeature(TermFragmentBuilder.createFeatureId(term.getTermType()))
            .orElseThrow(() -> new IllegalStateException("missing json term builder " + term.getTermType()));
        return builder.createFragments("data", column, term);
    }

    private RDBColumnMetadata jsonColumn(RDBSchemaMetadata schema, DataType type) {
        RDBTableMetadata table = schema.newTable("test_json_branch");
        RDBColumnMetadata column = table.newColumn();
        column.setName("data");
        column.setType(type);
        table.addColumn(column);
        return column;
    }

    private static class FallbackValueBuilder extends PostgresqlJsonTermFragmentBuilder {
        private final boolean not;

        private FallbackValueBuilder(boolean not) {
            super("json_value_negative", "json值反向查询", Operation.value, not);
            this.not = not;
        }

        private SqlFragments value(String columnFullName, RDBColumnMetadata column, JsonValueCondition condition) {
            return createValueFragments(columnFullName, column, condition, not);
        }
    }

    private static class FallbackContainsBuilder extends MysqlJsonTermFragmentBuilder {
        private final boolean not;

        private FallbackContainsBuilder(boolean not) {
            super("json_contains_by_value", "json包含值", Operation.contains, not);
            this.not = not;
        }

        private SqlFragments containsByValue(String columnFullName, RDBColumnMetadata column, Object value) {
            return createContainsByValueFragments(columnFullName, column, value, not);
        }
    }

    private record SimpleDataType(String id) implements DataType, DataTypeBuilder {
        @Override
        public String getId() {
            return id;
        }

        @Override
        public String getName() {
            return id;
        }

        @Override
        public Class<?> getJavaType() {
            return String.class;
        }

        @Override
        public SQLType getSqlType() {
            return JDBCType.VARCHAR;
        }

        @Override
        public String createColumnDataType(RDBColumnMetadata columnMetaData) {
            return id;
        }
    }
}
