package org.hswebframework.ezorm.core;

import lombok.Getter;
import org.hswebframework.ezorm.core.dsl.Query;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;
import org.junit.Assert;
import org.junit.Test;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;

public class NestConditionalLogicalOperationTest {

    @Test
    public void testNestConditionalCommonTerms() {
        Query<Object, QueryParam> query = Query.of();

        query.nest()
             .and("name", "JetLinks")
             .or("state", "enabled")
             .like("like", "abc")
             .like$("prefix", "abc")
             .$like("suffix", "abc")
             .$like$("contains", "abc")
             .notLike("notLike", "abc")
             .gt("gt", 1)
             .lt("lt", 2)
             .gte("gte", 3)
             .lte("lte", 4)
             .in("inArray", 1, 2)
             .in("inValue", 1)
             .in("inList", Arrays.asList(1, 2))
             .notIn("notIn", Arrays.asList(3, 4))
             .isEmpty("empty")
             .notEmpty("notEmpty")
             .isNull("isNull")
             .notNull("notNull")
             .not("not", 5)
             .between("between", 1, 10)
             .notBetween("notBetween", 2, 9)
             .end();

        Term nest = query.getParam().getTerms().get(0);
        Assert.assertEquals(Term.Type.and, nest.getType());
        Assert.assertEquals(22, nest.getTerms().size());
        assertTerm(nest.getTerms().get(0), Term.Type.and, "name", TermType.eq, "JetLinks");
        assertTerm(nest.getTerms().get(1), Term.Type.or, "state", TermType.eq, "enabled");
        assertTerm(nest.getTerms().get(3), Term.Type.or, "prefix", TermType.like, "abc%");
        assertTerm(nest.getTerms().get(4), Term.Type.or, "suffix", TermType.like, "%abc");
        assertTerm(nest.getTerms().get(5), Term.Type.or, "contains", TermType.like, "%abc%");
        Assert.assertEquals(Arrays.asList(1, 10), nest.getTerms().get(20).getValue());
    }

    @Test
    public void testNestedNestAndNullLikeValues() {
        Query<Object, QueryParam> query = Query.of();

        NestConditional<Query<Object, QueryParam>> root = query.orNest();
        root.like$("prefix", null)
            .$like("suffix", null)
            .$like$("contains", null)
            .nest("child", "C")
            .is("childName", "N")
            .end()
            .orNest("orChild", "O")
            .is("orChildName", "ON")
            .end()
            .end();

        Term rootTerm = query.getParam().getTerms().get(0);
        Assert.assertEquals(Term.Type.or, rootTerm.getType());
        Assert.assertNull(rootTerm.getColumn());
        Assert.assertNull(rootTerm.getValue());
        Assert.assertEquals(5, rootTerm.getTerms().size());
        assertTerm(rootTerm.getTerms().get(0), Term.Type.and, "prefix", TermType.like, null);
        assertTerm(rootTerm.getTerms().get(1), Term.Type.and, "suffix", TermType.like, null);
        assertTerm(rootTerm.getTerms().get(2), Term.Type.and, "contains", TermType.like, null);
        Assert.assertEquals(Term.Type.and, rootTerm.getTerms().get(3).getType());
        Assert.assertEquals(Term.Type.or, rootTerm.getTerms().get(4).getType());
    }

    @Test
    public void testLogicalOperationEachWhenAndAcceptHelpers() {
        Query<Object, QueryParam> query = Query.of();
        Map<String, Object> values = new LinkedHashMap<>();
        values.put("name", "JetLinks");
        values.put("age", 18);
        AtomicInteger consumerCount = new AtomicInteger();

        query.and((java.util.function.Supplier<Term>) () -> Term.of("supplierAnd", TermType.eq, "A"))
             .or((java.util.function.Supplier<Term>) () -> Term.of("supplierOr", TermType.eq, "B"))
             .each(Arrays.asList("A", "B"), (q, value) -> q.is("item", value))
             .each("tag", Arrays.asList("x", "y"), q -> q::like)
             .each("level", TermType.gt, Arrays.asList(1, 2), q -> q::and)
             .each("mapped", Arrays.asList(1, 2), q -> q::is, value -> "v" + value)
             .each(Entity::getName, Arrays.asList("J", "L"), Query::like, value -> value + "%")
             .each(values, Query::is)
             .when(true, q -> q.is("enabled", true))
             .when(false, q -> q.is("ignored", true))
             .when(() -> true, q -> q.is("supplied", true))
             .when(true, "score", Query::gt, 90)
             .when(false, "ignoredScore", Query::gt, 90)
             .when(true, Entity::getAge, Query::gte, 18)
             .when(true, Query::lt, methodColumn("age", 60))
             .when("state", "enabled", value -> value.startsWith("enable"), q -> q::is)
             .when("role", TermType.like, "admin", value -> value.startsWith("adm"), q -> q::and)
             .when(Optional.of("optional"), (q, value) -> q.is("optional", value))
             .when(Optional.empty(), (q, value) -> q.is("missingOptional", value))
             .accept(q -> consumerCount.incrementAndGet())
             .accept("accepted", (q, value) -> q.is("accepted", value))
             ;

        MethodReferenceColumn<String> method = NestConditionalLogicalOperationTest.<String>methodColumn("method", "value");
        if (method.get() != null) {
            query.is("method", method.get());
        }
        MethodReferenceColumn<String> nullMethod = NestConditionalLogicalOperationTest.<String>methodColumn("nullMethod", null);
        if (nullMethod.get() != null) {
            query.is("nullMethod", nullMethod.get());
        }

        Assert.assertSame(query, query.as(q -> q));
        Assert.assertEquals(1, consumerCount.get());
        Assert.assertEquals(24, query.getParam().getTerms().size());
        Assert.assertFalse(query.getParam().getTerms().stream().anyMatch(term -> "ignored".equals(term.getColumn())));
        Assert.assertFalse(query.getParam().getTerms().stream().anyMatch(term -> "missingOptional".equals(term.getColumn())));
        Assert.assertFalse(query.getParam().getTerms().stream().anyMatch(term -> "nullMethod".equals(term.getColumn())));
        Assert.assertEquals("v1", query.getParam().getTerms().get(8).getValue());
    }

    @Test
    public void testNullCollectionsAndMapsAreIgnored() {
        Query<Object, QueryParam> query = Query.of();

        query.each((java.util.Collection<String>) null, (q, value) -> q.is("ignored", value))
             .each("name", (java.util.Collection<String>) null, q -> q::like)
             .each("name", TermType.eq, (java.util.Collection<String>) null, q -> q::and)
             .each("name", (java.util.Collection<String>) null, q -> q::is, value -> value)
             .each((Map<String, Object>) null, Query::is)
             .each(Collections.emptyList(), value -> value, (q, value) -> q.is("ignored", value));

        Assert.assertTrue(query.getParam().getTerms().isEmpty());
    }


    @Test
    public void testNestConditionalMethodReferenceArrayJsonAndNullChecks() {
        Query<Object, QueryParam> query = Query.of();
        MethodReferenceColumn<String> name = methodColumn("name", "JetLinks");
        MethodReferenceColumn<Integer> age = methodColumn("age", 18);
        MethodReferenceColumn<Range> range = methodColumn("range", new Range(1, 9));

        NestConditional<Query<Object, QueryParam>> nest = query.nest();
        nest.is(staticColumn("product"), "gateway")
            .is(name)
            .like(staticColumn("name"), "Jet%")
            .like(name)
            .like$(name)
            .$like(name)
            .$like$(name)
            .like$(staticColumn("prefix"), "pre")
            .$like(staticColumn("suffix"), "suf")
            .$like$(staticColumn("contains"), "mid")
            .notLike(staticColumn("name"), "bad")
            .notLike(name)
            .gt(staticColumn("age"), 10)
            .gt(age)
            .lt(staticColumn("age"), 30)
            .lt(age)
            .gte(staticColumn("age"), 18)
            .gte(age)
            .lte(staticColumn("age"), 99)
            .lte(age)
            .in(staticColumn("state"), "enabled")
            .in(staticColumn("state"), "enabled", "disabled")
            .in(staticColumn("state"), Arrays.asList("enabled", "disabled"))
            .in(methodColumn("state", "enabled"))
            .notIn(staticColumn("state"), "removed")
            .notIn(methodColumn("state", "removed"))
            .contains(staticColumn("tags"), Collections.singleton("edge"))
            .contains(methodColumn("tags", Collections.singleton("edge")))
            .notContains(staticColumn("tags"), Collections.singleton("old"))
            .notContains(methodColumn("tags", Collections.singleton("old")))
            .contained(staticColumn("permissions"), Collections.singleton("read"))
            .contained(methodColumn("permissions", Collections.singleton("read")))
            .notContained(staticColumn("permissions"), Collections.singleton("root"))
            .notContained(methodColumn("permissions", Collections.singleton("root")))
            .overlap(staticColumn("areas"), Arrays.asList("A", "B"))
            .overlap(methodColumn("areas", Collections.singleton("C")))
            .notOverlap(staticColumn("areas"), Collections.singleton("Z"))
            .notOverlap(methodColumn("areas", Collections.singleton("Y")))
            .jsonExists("config", "$.enabled")
            .jsonContains("config", Collections.singletonMap("enabled", true))
            .jsonContained("config", Collections.singletonMap("version", 1))
            .jsonValue("config", "$.version", TermType.gte, 2)
            .jsonValue("config", "$.name", "JetLinks")
            .isEmpty(staticColumn("description"))
            .notEmpty(staticColumn("description"))
            .isNull(staticColumn("deletedTime"))
            .notNull(staticColumn("createdTime"))
            .not(staticColumn("state"), "disabled")
            .not(methodColumn("state", "disabled"))
            .between(range, Range::getStart, Range::getEnd)
            .between(staticColumn("age"), 1, 9)
            .notBetween(staticColumn("age"), 10, 99)
            .accept(staticColumn("custom"), "custom_term", "v")
            .accept(methodColumn("methodCustom", "mv"), "method_term");
        nest.and((java.util.function.Supplier<Term>) () -> Term.of("supplierAnd", TermType.eq, "A"));
        nest.or((java.util.function.Supplier<Term>) () -> Term.of("supplierOr", TermType.eq, "B"));
        nest.end();

        Term root = query.getParam().getTerms().get(0);
        Assert.assertEquals(56, root.getTerms().size());
        Assert.assertEquals("json_value", root.getTerms().get(41).getTermType());
        Assert.assertEquals(TermType.empty, root.getTerms().get(43).getTermType());
        Assert.assertEquals(TermType.nbtw, root.getTerms().get(51).getTermType());
        Assert.assertEquals("method_term", root.getTerms().get(53).getTermType());
        Assert.assertEquals(Term.Type.and, root.getTerms().get(54).getType());
        Assert.assertEquals(Term.Type.or, root.getTerms().get(55).getType());
    }

    private static <T> MethodReferenceColumn<T> methodColumn(String column, T value) {
        return new TestMethodReferenceColumn<>(column, value);
    }

    private static <T> StaticMethodReferenceColumn<T> staticColumn(String column) {
        return new TestStaticMethodReferenceColumn<>(column);
    }

    private static void assertTerm(Term term, Term.Type type, String column, String termType, Object value) {
        Assert.assertEquals(type, term.getType());
        Assert.assertEquals(column, term.getColumn());
        Assert.assertEquals(termType, term.getTermType());
        Assert.assertEquals(value, term.getValue());
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
    @lombok.AllArgsConstructor
    static class Range implements Serializable {
        private final int start;
        private final int end;
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

    @Getter
    static class Entity implements Serializable {
        private String name;
        private Integer age;
    }
}
