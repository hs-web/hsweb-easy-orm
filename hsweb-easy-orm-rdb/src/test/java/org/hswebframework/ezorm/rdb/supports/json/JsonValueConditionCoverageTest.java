package org.hswebframework.ezorm.rdb.supports.json;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;
import org.junit.Assert;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;

public class JsonValueConditionCoverageTest {

    @Test
    public void testParsingBranches() {
        Assert.assertEquals("name", JsonValueCondition.of(Term.of("x", JsonTermType.value, new LinkedHashMap<Object, Object>() {{ put("name", "name"); put("value", 1); }})).getPath());
        Assert.assertEquals(TermType.like, JsonValueCondition.of(Term.of("x", JsonTermType.value, new LinkedHashMap<Object, Object>() {{ put("path", "name"); put("op", TermType.like); put("val", "Jet%"); }})).getTermType());
        Assert.assertEquals("name", JsonValueCondition.of(Term.of("x", JsonTermType.value, Arrays.asList("name", 1, TermType.gt))).getPath());
        Assert.assertEquals(TermType.eq, JsonValueCondition.of(Term.of("x", JsonTermType.value, Collections.singletonList("name"))).getTermType());
        Assert.assertEquals(TermType.nin, JsonValueCondition.of(Term.of("x", JsonTermType.value, new Object[]{"state", "a,b", TermType.nin})).getTermType());
        Assert.assertEquals(TermType.eq, JsonValueCondition.of(Term.of("x", JsonTermType.value, "raw")).getTermType());
    }
}
