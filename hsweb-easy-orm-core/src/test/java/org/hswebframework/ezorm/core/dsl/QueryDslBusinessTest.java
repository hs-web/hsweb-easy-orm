package org.hswebframework.ezorm.core.dsl;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.hswebframework.ezorm.core.MethodReferenceColumn;
import org.hswebframework.ezorm.core.StaticMethodReferenceColumn;
import org.hswebframework.ezorm.core.param.Param;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.core.param.Sort;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;
import org.junit.Assert;
import org.junit.Test;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class QueryDslBusinessTest {

    @Test
    public void testQueryOptionsAndExecutionContract() {
        Query<Object, QueryParam> query = Query.of();

        Integer termCount = query.select("id", "name")
                                 .selectExcludes("password")
                                 .includes(methodColumn("runtime", "online"))
                                 .excludes(methodColumn("secret", "x"))
                                 .orderByAsc("name")
                                 .orderByDesc("createTime")
                                 .orderBy("score", sort -> sort.desc().function("abs").option("nulls", "last").value(0))
                                 .doPaging(2, 20)
                                 .noPaging()
                                 .forUpdate()
                                 .strictTerm(true)
                                 .where("id", "dev1")
                                 .execute(param -> param.getTerms().size());

        QueryParam param = query.getParam();
        Assert.assertEquals(Integer.valueOf(1), termCount);
        Assert.assertTrue(param.getIncludes().contains("id"));
        Assert.assertTrue(param.getIncludes().contains("runtime"));
        Assert.assertTrue(param.getExcludes().contains("password"));
        Assert.assertTrue(param.getExcludes().contains("secret"));
        Assert.assertFalse(param.isPaging());
        Assert.assertTrue(param.isForUpdate());
        Assert.assertTrue(param.isStrictTerm());
        Assert.assertEquals(Boolean.TRUE, param.getContext(QueryParam.STRICT_TERM_KEY).orElse(null));
        Assert.assertEquals(3, param.getSorts().size());
        Assert.assertEquals("asc", param.getSorts().get(0).getOrder());
        Assert.assertEquals("desc", param.getSorts().get(1).getOrder());
        Sort functionSort = param.getSorts().get(2);
        Assert.assertEquals("abs", functionSort.getType());
        Assert.assertEquals("last", functionSort.getOpts().get("nulls"));
        Assert.assertEquals(0, functionSort.getValue());
    }

    @Test
    public void testConditionalStringAndMethodReferenceTerms() {
        Query<Object, QueryParam> query = Query.of();
        MethodReferenceColumn<String> name = methodColumn("name", "JetLinks");
        MethodReferenceColumn<Integer> age = methodColumn("age", 18);
        MethodReferenceColumn<Range> range = methodColumn("range", new Range(10, 20));

        query.where()
             .where(q -> q.where("tenantId", "t1"))
             .where(name)
             .and(staticColumn("state"), TermType.eq, "enabled")
             .or(staticColumn("type"), TermType.eq, "device")
             .and(name)
             .or(age)
             .is(staticColumn("productId"), "prod1")
             .is(name)
             .like(name)
             .like$(name)
             .$like(name)
             .$like$(name)
             .notLike(name)
             .gt(age)
             .lt(age)
             .gte(age)
             .lte(age)
             .in(age)
             .notIn(age)
             .between(range, Range::getStart, Range::getEnd)
             .notBetween(range, Range::getStart, Range::getEnd)
             .accept(staticColumn("custom"), "custom_term", "v")
             .accept(methodColumn("methodCustom", "v2"), "custom_method");

        Assert.assertEquals(23, query.getParam().getTerms().size());
        Assert.assertEquals(Arrays.asList("tenantId", "name", "state", "type"), columns(query.getParam().getTerms()).subList(0, 4));
        Assert.assertEquals(Term.Type.or, query.getParam().getTerms().get(3).getType());
        Assert.assertEquals(TermType.like, query.getParam().getTerms().get(10).getTermType());
        Assert.assertEquals("JetLinks%", query.getParam().getTerms().get(9).getValue());
        Assert.assertEquals("%JetLinks", query.getParam().getTerms().get(10).getValue());
        Assert.assertEquals("%JetLinks%", query.getParam().getTerms().get(11).getValue());
        Assert.assertEquals(Arrays.asList(10, 20), query.getParam().getTerms().get(19).getValue());
        Assert.assertEquals(TermType.nbtw, query.getParam().getTerms().get(20).getTermType());
        Assert.assertEquals("custom_term", query.getParam().getTerms().get(21).getTermType());
        Assert.assertEquals("custom_method", query.getParam().getTerms().get(22).getTermType());
    }

    @Test
    public void testArrayAndJsonTermsForRepositoryQueries() {
        Query<Object, QueryParam> query = Query.of();

        query.contains(staticColumn("tags"), Collections.singleton("gateway"))
             .contains(methodColumn("tags", Collections.singleton("edge")))
             .notContains(staticColumn("tags"), Collections.singleton("deprecated"))
             .notContains(methodColumn("tags", Collections.singleton("legacy")))
             .contained(staticColumn("permissions"), Arrays.asList("read", "write"))
             .contained(methodColumn("permissions", Collections.singleton("read")))
             .notContained(staticColumn("permissions"), Collections.singleton("root"))
             .notContained(methodColumn("permissions", Collections.singleton("guest")))
             .overlap(staticColumn("areas"), Arrays.asList("A", "B"))
             .overlap(methodColumn("areas", Collections.singleton("C")))
             .notOverlap(staticColumn("areas"), Collections.singleton("Z"))
             .notOverlap(methodColumn("areas", Collections.singleton("Y")))
             .jsonExists("configuration", "$.network")
             .jsonContains("configuration", Collections.singletonMap("enabled", true))
             .jsonContained("configuration", Collections.singletonMap("version", 1))
             .jsonValue("configuration", "$.version", TermType.gte, 2)
             .jsonValue("configuration", "$.name", "JetLinks");

        Assert.assertEquals(17, query.getParam().getTerms().size());
        Assert.assertEquals(TermType.contains, query.getParam().getTerms().get(0).getTermType());
        Assert.assertEquals(TermType.ncontains, query.getParam().getTerms().get(2).getTermType());
        Assert.assertEquals(TermType.contained, query.getParam().getTerms().get(4).getTermType());
        Assert.assertEquals(TermType.noverlap, query.getParam().getTerms().get(10).getTermType());
        Assert.assertEquals("json_exists", query.getParam().getTerms().get(12).getTermType());
        Map<?, ?> value = (Map<?, ?>) query.getParam().getTerms().get(15).getValue();
        Assert.assertEquals("$.version", value.get("path"));
        Assert.assertEquals(TermType.gte, value.get("termType"));
        Assert.assertEquals(2, value.get("value"));
        Assert.assertEquals(TermType.eq, ((Map<?, ?>) query.getParam().getTerms().get(16).getValue()).get("termType"));
    }

    @Test
    public void testAcceptParamAndNullConditionsAreIgnoredByQuery() {
        Param param = new Param();
        param.and("id", TermType.eq, "dev1");
        param.or("name", TermType.like, "%gateway%");

        Query<Object, QueryParam> query = Query.of()
                                               .where("ignored", TermType.eq, null)
                                               .and("ignoredAnd", TermType.eq, null)
                                               .or("ignoredOr", TermType.eq, null)
                                               .like$("nullableLike", null)
                                               .accept(param);

        Assert.assertEquals(2, query.getParam().getTerms().size());
        Assert.assertEquals("id", query.getParam().getTerms().get(0).getColumn());
        Assert.assertEquals(Term.Type.or, query.getParam().getTerms().get(1).getType());
    }

    private static List<String> columns(List<Term> terms) {
        return terms.stream().map(Term::getColumn).collect(Collectors.toList());
    }

    private static <T> MethodReferenceColumn<T> methodColumn(String column, T value) {
        return new TestMethodReferenceColumn<>(column, value);
    }

    private static <T> StaticMethodReferenceColumn<T> staticColumn(String column) {
        return new TestStaticMethodReferenceColumn<>(column);
    }

    static class TestMethodReferenceColumn<T> implements MethodReferenceColumn<T> {
        private final String column;
        private final T value;

        TestMethodReferenceColumn(String column, T value) {
            this.column = column;
            this.value = value;
        }

        @Override
        public String getColumn() {
            return column;
        }

        @Override
        public T get() {
            return value;
        }
    }

    static class TestStaticMethodReferenceColumn<T> implements StaticMethodReferenceColumn<T> {
        private final String column;

        TestStaticMethodReferenceColumn(String column) {
            this.column = column;
        }

        @Override
        public String getColumn() {
            return column;
        }

        @Override
        public Object apply(T value) {
            return null;
        }
    }

    @Getter
    @AllArgsConstructor
    static class Range implements Serializable {
        private final int start;
        private final int end;
    }
}
