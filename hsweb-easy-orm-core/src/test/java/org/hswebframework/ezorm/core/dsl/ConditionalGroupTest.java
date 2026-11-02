package org.hswebframework.ezorm.core.dsl;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.NestConditional;
import org.hswebframework.ezorm.core.param.Param;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.core.param.Term;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

public class ConditionalGroupTest {
    @Test
    public void queryPreservesOptionsAndGroupsExistingConditions() {
        Query<Object, QueryParam> query = Query.of();
        query.includes("id").excludes("name").doPaging(1, 5).orderByAsc("id");
        query.getParam().context("trace", "unchanged");
        query.is("a", 1).or("b", 2);
        assertSame(query, query.group().nest().is("c", 3).end());
        assertGrouped(query.getParam());
        assertEquals(1, query.getParam().getPageIndex());
        assertEquals(5, query.getParam().getPageSize());
        assertTrue(query.getParam().getIncludes().contains("id"));
        assertTrue(query.getParam().getExcludes().contains("name"));
        assertEquals(1, query.getParam().getSorts().size());
        assertEquals("unchanged", query.getParam().getContext("trace").orElse(null));
    }

    @Test
    public void updatePreservesDataAndColumns() {
        var update = Update.of(Map.of("name", "changed"));
        update.includes("name").excludes("state").is("a", 1).or("b", 2);
        assertSame(update, update.group().nest().is("c", 3).end());
        assertGrouped(update.getParam());
        assertEquals("changed", update.getParam().getData().get("name"));
        assertTrue(update.getParam().getIncludes().contains("name"));
        assertTrue(update.getParam().getExcludes().contains("state"));
    }

    @Test
    public void deleteGroupsExistingConditions() {
        var delete = Delete.of().is("a", 1).or("b", 2);
        assertSame(delete, delete.group().nest().is("c", 3).end());
        assertGrouped(delete.getParam());
    }

    @Test
    public void readOnlyEmptySingleAndMultipleConditionsAllowFurtherAppend() {
        for (int size = 0; size <= 2; size++) {
            QueryParam param = new QueryParam();
            var source = List.of(Term.of("a", "eq", 1), Term.of("b", "eq", 2));
            param.setTerms(source.subList(0, size));
            var query = Query.of(param);
            query.group();
            var grouped = List.copyOf(param.getTerms());
            query.group().group();
            assertEquals(grouped, param.getTerms());
            query.nest().is("c", 3).end();
            assertEquals(size == 0 ? 1 : 2, param.getTerms().size());
            assertEquals(2, source.size());
        }
    }

    @Test
    public void groupingDoesNotRetainTheCallersMutableList() {
        var source = new ArrayList<>(List.of(Term.of("a", "eq", 1), Term.of("b", "eq", 2)));
        var param = new QueryParam();
        param.setTerms(source);
        Query.of(param).group();
        source.clear();
        assertEquals(2, param.getTerms().get(0).getTerms().size());
    }

    @Test
    public void nestStillOnlyAppendsANewGroup() {
        var query = Query.of().is("a", 1).or("b", 2);
        query.nest().is("c", 3).end();
        assertEquals(3, query.getParam().getTerms().size());
        assertEquals("a", query.getParam().getTerms().get(0).getColumn());
        assertEquals(Term.Type.or, query.getParam().getTerms().get(1).getType());
    }

    @Test
    public void unadaptedImplementationCannotSilentlySkipGrouping() {
        assertThrows(UnsupportedOperationException.class, () -> new UnadaptedConditional().group());
    }

    private static void assertGrouped(Param param) {
        // 根条件分组后追加的 nest 与业务 OR 分支保持独立。
        var terms = param.getTerms();
        assertEquals(2, terms.size());
        assertEquals(Term.Type.and, terms.get(0).getType());
        assertEquals(2, terms.get(0).getTerms().size());
        assertEquals("a", terms.get(0).getTerms().get(0).getColumn());
        assertEquals(Term.Type.or, terms.get(0).getTerms().get(1).getType());
        assertEquals("b", terms.get(0).getTerms().get(1).getColumn());
        assertEquals(Term.Type.and, terms.get(1).getType());
        assertEquals("c", terms.get(1).getTerms().get(0).getColumn());
    }

    @Test
    public void nestedGroupPreservesOuterOrAndParentReturn() {
        var query = Query.of().is("outside", 0);
        var nested = query.orNest().is("a", 1).or("b", 2);
        assertSame(nested, nested.group());
        nested.and("c", 3);
        var term = query.getParam().getTerms().get(1);
        assertEquals(Term.Type.or, term.getType());
        assertEquals(2, term.getTerms().size());
        assertEquals(2, term.getTerms().get(0).getTerms().size());
        assertEquals(Term.Type.or, term.getTerms().get(0).getTerms().get(1).getType());
        assertEquals("c", term.getTerms().get(1).getColumn());
        assertEquals(Term.Type.and, term.getTerms().get(1).getType());
        assertSame(query, nested.end());
    }

    @Test
    public void deepGroupsReturnEachOriginalParent() {
        var query = Query.of();
        var outer = query.nest();
        var inner = outer.nest();
        assertSame(inner, inner.is("a", 1).or("b", 2).group());
        assertSame(outer, inner.and("c", 3).end());
        assertSame(query, outer.end());
    }

    @Test
    public void nestedReadOnlyConditionsAllowRepeatedGroupingAndAppend() {
        for (int size = 0; size <= 2; size++) {
            var query = Query.of();
            var nested = query.nest();
            var term = query.getParam().getTerms().get(0);
            var source = List.of(Term.of("a", "eq", 1), Term.of("b", "eq", 2)).subList(0, size);
            term.setTerms(source);
            nested.group();
            var grouped = List.copyOf(term.getTerms());
            nested.group().group();
            assertEquals(grouped, term.getTerms());
            assertSame(nested, nested.and("c", 3));
            assertEquals(size == 0 ? 1 : 2, term.getTerms().size());
            assertEquals(size, source.size());
        }
    }

    @Test
    public void nestedGroupingKeepsCarrierMetadataAndSnapshotsTheChildList() {
        var query = Query.of();
        var outer = query.nest();
        var nested = outer.orNest("anchor", 7);
        var term = query.getParam().getTerms().get(0).getTerms().get(0);
        var source = new ArrayList<>(List.of(Term.of("a", "eq", 1), Term.of("b", "eq", 2)));
        term.setTerms(source);
        term.getOptions().add("retained");
        assertSame(nested, nested.group());
        source.clear();
        assertEquals("anchor", term.getColumn());
        assertEquals(7, term.getValue());
        assertEquals(Term.Type.or, term.getType());
        assertEquals(List.of("retained"), term.getOptions());
        assertEquals(2, term.getTerms().get(0).getTerms().size());
        assertSame(outer, nested.end());
    }

    private static final class UnadaptedConditional implements Conditional<UnadaptedConditional> {
        public NestConditional<UnadaptedConditional> nest() { throw new UnsupportedOperationException(); }
        public NestConditional<UnadaptedConditional> orNest() { throw new UnsupportedOperationException(); }
        public UnadaptedConditional and() { return this; }
        public UnadaptedConditional or() { return this; }
        public UnadaptedConditional and(String column, String type, Object value) { return this; }
        public UnadaptedConditional or(String column, String type, Object value) { return this; }
        public UnadaptedConditional accept(Term term) { return this; }
        public Accepter<UnadaptedConditional, Object> getAccepter() { return this::and; }
    }
}
