package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SimpleTermsFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.junit.Assert;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

public class PostgresqlJsonbExistBuilderCoverageTest {

    private final PostgresqlJsonbExistTermFragmentBuilder builder = PostgresqlJsonbExistTermFragmentBuilder.exist;

    @Test
    public void testBaseExistAcceptsNativeAndPlainPathValues() {
        RDBColumnMetadata column = jsonbColumn(null);

        SqlRequest nativePath = create(column, Term.of("metadata", "exist", NativeSql.of("lower(?)", "name"))).toRequest();
        Assert.assertEquals("metadata ? lower(?)", nativePath.getSql());
        Assert.assertArrayEquals(new Object[]{"name"}, nativePath.getParameters());

        SqlRequest plainPath = create(column, Term.of("metadata", "exist", "profile.name")).toRequest();
        Assert.assertEquals("metadata ? ?", plainPath.getSql());
        Assert.assertArrayEquals(new Object[]{"profile.name"}, plainPath.getParameters());
    }

    @Test
    public void testAllAnyOptionsConvertListLikeValuesAndSkipEmptyValues() {
        RDBColumnMetadata column = jsonbColumn(null);

        SqlRequest allFromString = create(column, Term.of("metadata", "exist", "name,age", "all")).toRequest();
        Assert.assertEquals("metadata ?& array[ ?,? ]", allFromString.getSql());
        Assert.assertArrayEquals(new Object[]{"name", "age"}, allFromString.getParameters());

        SqlRequest anyFromArray = create(column, Term.of("metadata", "exist", new Object[]{"name", "age"}, "any")).toRequest();
        Assert.assertEquals("metadata ?| array[ ?,? ]", anyFromArray.getSql());
        Assert.assertArrayEquals(new Object[]{"name", "age"}, anyFromArray.getParameters());

        SqlRequest anyFromCollection = create(column, Term.of("metadata", "exist", Arrays.asList("name", "age"), "any")).toRequest();
        Assert.assertEquals("metadata ?| array[ ?,? ]", anyFromCollection.getSql());
        Assert.assertArrayEquals(new Object[]{"name", "age"}, anyFromCollection.getParameters());

        Assert.assertTrue(create(column, Term.of("metadata", "exist", null, "all")).isEmpty());
        Assert.assertTrue(create(column, Term.of("metadata", "exist", Collections.emptyList(), "any")).isEmpty());
    }

    @Test
    public void testContainsAndContainedEncodeObjectValuesAsJsonb() {
        RDBColumnMetadata column = jsonbColumn(null);
        Map<String, Object> value = new LinkedHashMap<>();
        value.put("name", "JetLinks");

        SqlRequest contains = create(column, Term.of("metadata", "exist", value, "contains")).toRequest();
        Assert.assertEquals("metadata @> ?::jsonb", contains.getSql());
        Assert.assertEquals("{\"name\":\"JetLinks\"}", contains.getParameters()[0]);

        SqlRequest contained = create(column, Term.of("metadata", "exist", "{\"name\":\"JetLinks\"}", "contained")).toRequest();
        Assert.assertEquals("metadata <@ ?::jsonb", contained.getSql());
        Assert.assertEquals("{\"name\":\"JetLinks\"}", contained.getParameters()[0]);

        SqlRequest nullContains = create(column, Term.of("metadata", "exist", null, "contains")).toRequest();
        Assert.assertEquals("metadata @> ?", nullContains.getSql());
        Assert.assertArrayEquals(new Object[]{null}, nullContains.getParameters());
    }

    @Test
    public void testContainsUsesColumnValueCodecBeforeJsonEncoding() {
        RDBColumnMetadata nativeCodecColumn = jsonbColumn(new TestCodec(NativeSql.of("jsonb_build_object(?,?)", "name", "JetLinks")));
        SqlRequest nativeEncoded = create(nativeCodecColumn, Term.of("metadata", "exist", Collections.singletonMap("name", "ignored"), "contains")).toRequest();
        Assert.assertEquals("metadata @> jsonb_build_object(?,?)", nativeEncoded.getSql());
        Assert.assertArrayEquals(new Object[]{"name", "JetLinks"}, nativeEncoded.getParameters());

        RDBColumnMetadata jsonCodecColumn = jsonbColumn(new TestCodec("{\"encoded\":true}"));
        SqlRequest jsonEncoded = create(jsonCodecColumn, Term.of("metadata", "exist", Collections.singletonMap("name", "ignored"), "contained")).toRequest();
        Assert.assertEquals("metadata <@ ?::jsonb", jsonEncoded.getSql());
        Assert.assertEquals("{\"encoded\":true}", jsonEncoded.getParameters()[0]);

        RDBColumnMetadata nullCodecColumn = jsonbColumn(new TestCodec(null));
        SqlRequest nullEncoded = create(nullCodecColumn, Term.of("metadata", "exist", Collections.singletonMap("name", "ignored"), "contains")).toRequest();
        Assert.assertEquals("metadata @> ?", nullEncoded.getSql());
        Assert.assertArrayEquals(new Object[]{null}, nullEncoded.getParameters());
    }


    @Test
    public void testJsonbCommonTermTypeDefaults() {
        RDBColumnMetadata column = jsonbColumn(null);
        Map<String, Object> value = new LinkedHashMap<>();
        value.put("name", "JetLinks");

        SqlRequest contains = PostgresqlJsonbTermFragmentBuilder.contains
            .createFragments("metadata", column, Term.of("metadata", "contains", value))
            .toRequest();
        Assert.assertEquals("metadata @> ?::jsonb", contains.getSql());
        Assert.assertEquals("{\"name\":\"JetLinks\"}", contains.getParameters()[0]);

        SqlRequest contained = PostgresqlJsonbTermFragmentBuilder.contained
            .createFragments("metadata", column, Term.of("metadata", "contained", value))
            .toRequest();
        Assert.assertEquals("metadata <@ ?::jsonb", contained.getSql());
        Assert.assertEquals("{\"name\":\"JetLinks\"}", contained.getParameters()[0]);

        SqlRequest in = PostgresqlJsonbTermFragmentBuilder.in
            .createFragments("metadata", column, Term.of("metadata", "in", Arrays.asList("name", "age")))
            .toRequest();
        Assert.assertEquals("metadata ?| array[ ?,? ]", in.getSql());
        Assert.assertArrayEquals(new Object[]{"name", "age"}, in.getParameters());

        SqlRequest overlap = PostgresqlJsonbTermFragmentBuilder.overlap
            .createFragments("metadata", column, Term.of("metadata", "overlap", "name,age"))
            .toRequest();
        Assert.assertEquals("metadata ?| array[ ?,? ]", overlap.getSql());
        Assert.assertArrayEquals(new Object[]{"name", "age"}, overlap.getParameters());
    }

    @Test
    public void testJsonbCommonTermTypeOptionsOverrideDefaultBehavior() {
        RDBColumnMetadata column = jsonbColumn(null);
        Map<String, Object> value = new LinkedHashMap<>();
        value.put("name", "JetLinks");

        SqlRequest containsAllKeys = PostgresqlJsonbTermFragmentBuilder.contains
            .createFragments("metadata", column, Term.of("metadata", "contains", Arrays.asList("name", "age"), "all"))
            .toRequest();
        Assert.assertEquals("metadata ?& array[ ?,? ]", containsAllKeys.getSql());
        Assert.assertArrayEquals(new Object[]{"name", "age"}, containsAllKeys.getParameters());

        SqlRequest inJsonContains = PostgresqlJsonbTermFragmentBuilder.in
            .createFragments("metadata", column, Term.of("metadata", "in", value, "json"))
            .toRequest();
        Assert.assertEquals("metadata @> ?::jsonb", inJsonContains.getSql());
        Assert.assertEquals("{\"name\":\"JetLinks\"}", inJsonContains.getParameters()[0]);

        SqlRequest containsKey = PostgresqlJsonbTermFragmentBuilder.contains
            .createFragments("metadata", column, Term.of("metadata", "contains", "name", "key"))
            .toRequest();
        Assert.assertEquals("metadata ? ?", containsKey.getSql());
        Assert.assertArrayEquals(new Object[]{"name"}, containsKey.getParameters());

        SqlRequest negative = PostgresqlJsonbTermFragmentBuilder.notContains
            .createFragments("metadata", column, Term.of("metadata", "ncontains", value))
            .toRequest();
        Assert.assertEquals("( metadata is null or not ( metadata @> ?::jsonb ) )", negative.getSql());
        Assert.assertEquals("{\"name\":\"JetLinks\"}", negative.getParameters()[0]);
    }

    @Test
    public void testJsonbCommonTermTypeRegisteredOnJsonbColumn() {
        RDBColumnMetadata column = jsonbColumn(null);
        Assert.assertTrue(column.findFeature(org.hswebframework.ezorm.rdb.operator.builder.fragments.TermFragmentBuilder.createFeatureId("contains")).isPresent());
        Assert.assertTrue(column.findFeature(org.hswebframework.ezorm.rdb.operator.builder.fragments.TermFragmentBuilder.createFeatureId("ncontains")).isPresent());
        Assert.assertTrue(column.findFeature(org.hswebframework.ezorm.rdb.operator.builder.fragments.TermFragmentBuilder.createFeatureId("in")).isPresent());
        Assert.assertTrue(column.findFeature(org.hswebframework.ezorm.rdb.operator.builder.fragments.TermFragmentBuilder.createFeatureId("overlap")).isPresent());
    }



    @Test
    public void testJsonbCommonTermTypeWorksThroughColumnFeatureLookup() {
        RDBTableMetadata table = jsonbTable(null);

        Term containsAll = new Term();
        containsAll.setColumn("metadata$contains$all");
        containsAll.setValue("name,age");
        SqlRequest containsAllRequest = SimpleTermsFragmentBuilder.createByTable(table, containsAll).toRequest();
        Assert.assertEquals("\"metadata\" ?& array[ ?,? ]", containsAllRequest.getSql());
        Assert.assertArrayEquals(new Object[]{"name", "age"}, containsAllRequest.getParameters());

        Term notOverlap = new Term();
        notOverlap.setColumn("metadata$noverlap");
        notOverlap.setValue(Arrays.asList("name", "status"));
        SqlRequest notOverlapRequest = SimpleTermsFragmentBuilder.createByTable(table, notOverlap).toRequest();
        Assert.assertEquals("( \"metadata\" is null or not ( \"metadata\" ?| array[ ?,? ] ) )", notOverlapRequest.getSql());
        Assert.assertArrayEquals(new Object[]{"name", "status"}, notOverlapRequest.getParameters());
    }

    @Test
    public void testOptionPriorityMatchesRepositoryColumnSyntax() {
        Term term = Term.of("metadata", "exist", Collections.singletonMap("name", "JetLinks"), "any", "all", "contained", "contains");
        Assert.assertEquals("@>", PostgresqlJsonbExistTermFragmentBuilder.getOperator(term));

        Term columnSyntax = new Term();
        columnSyntax.setColumn("metadata$exist$all");
        columnSyntax.setValue("name,age");
        SqlRequest request = create(jsonbColumn(null), columnSyntax).toRequest();
        Assert.assertEquals("metadata ?& array[ ?,? ]", request.getSql());
        Assert.assertArrayEquals(new Object[]{"name", "age"}, request.getParameters());
    }

    private SqlFragments create(RDBColumnMetadata column, Term term) {
        return builder.createFragments("metadata", column, term);
    }

    private static RDBColumnMetadata jsonbColumn(ValueCodec<Object, Object> codec) {
        return jsonbTable(codec).getColumn("metadata").orElseThrow();
    }

    private static RDBTableMetadata jsonbTable(ValueCodec<Object, Object> codec) {
        PostgresqlSchemaMetadata schema = new PostgresqlSchemaMetadata("public");
        RDBTableMetadata table = schema.newTable("test_jsonb_exist");
        RDBColumnMetadata column = table.newColumn();
        column.setName("metadata");
        column.setType(JsonbType.INSTANCE);
        column.setValueCodec(codec);
        table.addColumn(column);
        return table;
    }

    private static class TestCodec implements ValueCodec<Object, Object> {
        private final Object encoded;

        private TestCodec(Object encoded) {
            this.encoded = encoded;
        }

        @Override
        public Object encode(Object value) {
            return encoded;
        }

        @Override
        public Object decode(Object data) {
            return data;
        }
    }
}
