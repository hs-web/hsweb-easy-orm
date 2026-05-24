package org.hswebframework.ezorm.rdb.supports.json;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.TermFragmentBuilder;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlDialect;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.oracle.OracleSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.postgres.JsonbType;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlSchemaMetadata;
import org.junit.Assert;
import org.junit.Test;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

public class JsonTermFragmentBuilderTest {

    @Test
    public void testMysqlJsonTypeRegistration() {
        MysqlDialect dialect = new MysqlDialect();
        Assert.assertTrue(dialect.convertDataType("json") instanceof JsonDataType);
    }

    @Test
    public void testPostgresqlJsonTerms() {
        RDBColumnMetadata column = jsonColumn(new PostgresqlSchemaMetadata("public"), JsonbType.INSTANCE);

        SqlRequest exists = create(column, Term.of("data", JsonTermType.exists, "$.detail.name")).toRequest();
        Assert.assertEquals("( data ::jsonb #> array[?,?] ) is not null", exists.getSql());
        Assert.assertArrayEquals(new Object[]{"detail", "name"}, exists.getParameters());

        SqlRequest contains = create(column, Term.of("data", JsonTermType.contains, Collections.singletonMap("name", "JetLinks"))).toRequest();
        Assert.assertEquals("data ::jsonb @> ?::jsonb", contains.getSql());
        Assert.assertEquals("{\"name\":\"JetLinks\"}", contains.getParameters()[0]);

        SqlRequest value = create(column, Term.of("data", JsonTermType.value, JsonValueCondition.of("age", TermType.gt, 18))).toRequest();
        Assert.assertEquals("cast((data::jsonb #>> array[?]) as numeric) > ?", value.getSql());
        Assert.assertArrayEquals(new Object[]{"age", 18}, value.getParameters());
    }

    @Test
    public void testMysqlJsonTerms() {
        RDBColumnMetadata column = jsonColumn(new MysqlSchemaMetadata("test"), JsonType.INSTANCE);

        SqlRequest exists = create(column, Term.of("data", JsonTermType.exists, "name")).toRequest();
        Assert.assertEquals("json_contains_path( data ,'one', ? )", exists.getSql());
        Assert.assertArrayEquals(new Object[]{"$.name"}, exists.getParameters());

        SqlRequest contains = create(column, Term.of("data", JsonTermType.contains, Collections.singletonMap("name", "JetLinks"))).toRequest();
        Assert.assertEquals("json_contains( data , ? )", contains.getSql());
        Assert.assertEquals("{\"name\":\"JetLinks\"}", contains.getParameters()[0]);

        SqlRequest value = create(column, Term.of("data", JsonTermType.value, JsonValueCondition.of("$.name", "JetLinks"))).toRequest();
        Assert.assertEquals("json_unquote(json_extract(data,?)) = ?", value.getSql());
        Assert.assertArrayEquals(new Object[]{"$.name", "JetLinks"}, value.getParameters());
    }

    @Test
    public void testOracleContainsFallback() {
        RDBColumnMetadata column = jsonColumn(new OracleSchemaMetadata("PUBLIC"), JsonType.CLOB);
        Map<String, Object> target = new LinkedHashMap<>();
        target.put("name", "JetLinks");
        target.put("age", 18);

        SqlRequest request = create(column, Term.of("data", JsonTermType.contains, target)).toRequest();
        Assert.assertEquals("( json_value(data,?) = ? and json_value(data,? returning number) = ? )", request.getSql());
        Assert.assertArrayEquals(new Object[]{"$.name", "JetLinks", "$.age", 18}, request.getParameters());
    }

    @Test(expected = UnsupportedOperationException.class)
    public void testH2JsonQueryUnsupported() {
        RDBColumnMetadata column = jsonColumn(new org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata("PUBLIC"), JsonType.INSTANCE);
        create(column, Term.of("data", JsonTermType.exists, "name")).toRequest();
    }

    private SqlFragments create(RDBColumnMetadata column, Term term) {
        TermFragmentBuilder builder = column
            .findFeature(TermFragmentBuilder.createFeatureId(term.getTermType()))
            .orElseThrow(() -> new IllegalStateException("missing json term builder " + term.getTermType()));
        return builder.createFragments("data", column, term);
    }

    private RDBColumnMetadata jsonColumn(RDBSchemaMetadata schema, org.hswebframework.ezorm.rdb.metadata.DataType type) {
        RDBTableMetadata table = schema.newTable("test_json");
        RDBColumnMetadata column = table.newColumn();
        column.setName("data");
        column.setType(type);
        table.addColumn(column);
        return column;
    }
}
