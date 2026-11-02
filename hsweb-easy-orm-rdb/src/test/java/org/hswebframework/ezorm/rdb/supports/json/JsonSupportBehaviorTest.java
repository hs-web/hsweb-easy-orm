package org.hswebframework.ezorm.rdb.supports.json;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;
import org.junit.Assert;
import org.junit.Test;

import javax.sql.rowset.serial.SerialBlob;
import javax.sql.rowset.serial.SerialClob;
import java.io.ByteArrayInputStream;
import java.io.StringReader;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class JsonSupportBehaviorTest {

    @Test
    public void testJsonValueConditionAcceptsRepositoryDslShapes() {
        JsonValueCondition direct = JsonValueCondition.of("profile.age", TermType.gt, 18);
        Assert.assertSame(direct, JsonValueCondition.of(term(direct)));

        Map<String, Object> map = new LinkedHashMap<>();
        map.put("key", "profile.name");
        map.put("operator", TermType.like);
        map.put("val", "JetLinks%");
        JsonValueCondition fromMap = JsonValueCondition.of(term(map));
        Assert.assertEquals("profile.name", fromMap.getPath());
        Assert.assertEquals(TermType.like, fromMap.getTermType());
        Assert.assertEquals("JetLinks%", fromMap.getValue());

        Map<String, Object> mapWithDefaultTerm = new LinkedHashMap<>();
        mapWithDefaultTerm.put("name", "profile.score");
        mapWithDefaultTerm.put("value", 90);
        Term optionTerm = term(mapWithDefaultTerm);
        optionTerm.getOptions().add(TermType.gte);
        JsonValueCondition fromMapDefault = JsonValueCondition.of(optionTerm);
        Assert.assertEquals("profile.score", fromMapDefault.getPath());
        Assert.assertEquals(TermType.gte, fromMapDefault.getTermType());
        Assert.assertEquals(90, fromMapDefault.getValue());

        JsonValueCondition fromCollection = JsonValueCondition.of(term(Arrays.asList("profile.level", 3, TermType.lte)));
        Assert.assertEquals("profile.level", fromCollection.getPath());
        Assert.assertEquals(TermType.lte, fromCollection.getTermType());
        Assert.assertEquals(3, fromCollection.getValue());

        JsonValueCondition fromShortCollection = JsonValueCondition.of(term(Collections.singletonList("profile.enabled")));
        Assert.assertEquals("profile.enabled", fromShortCollection.getPath());
        Assert.assertEquals(TermType.eq, fromShortCollection.getTermType());
        Assert.assertNull(fromShortCollection.getValue());

        JsonValueCondition fromArray = JsonValueCondition.of(term(new Object[]{"profile.age", 18, TermType.gt}));
        Assert.assertEquals("profile.age", fromArray.getPath());
        Assert.assertEquals(TermType.gt, fromArray.getTermType());
        Assert.assertEquals(18, fromArray.getValue());

        Term optionArray = term(new Object[]{"profile.age", 18});
        optionArray.getOptions().add(TermType.gte);
        Assert.assertEquals(TermType.gte, JsonValueCondition.of(optionArray).getTermType());

        JsonValueCondition raw = JsonValueCondition.of(term("raw"));
        Assert.assertNull(raw.getPath());
        Assert.assertEquals(TermType.eq, raw.getTermType());
        Assert.assertEquals("raw", raw.getValue());
    }

    @Test
    public void testJsonCodecSupportReadsCommonJdbcAndStreamValues() throws Exception {
        Assert.assertNull(JsonCodecSupport.toJson(null));
        Assert.assertEquals("{\"a\":1}", JsonCodecSupport.toJson("{\"a\":1}"));
        Assert.assertEquals("{\"a\":1}", JsonCodecSupport.toJsonSilently(Collections.singletonMap("a", 1)));
        Assert.assertFalse(JsonCodecSupport.canReadAsString(null));

        byte[] bytes = "{\"name\":\"bytes\"}".getBytes(StandardCharsets.UTF_8);
        ByteBuffer buffer = ByteBuffer.wrap(bytes);
        Assert.assertTrue(JsonCodecSupport.canReadAsString(bytes));
        Assert.assertTrue(JsonCodecSupport.canReadAsString(buffer));
        Assert.assertEquals("{\"name\":\"bytes\"}", JsonCodecSupport.readAsString(bytes));
        Assert.assertEquals("{\"name\":\"bytes\"}", JsonCodecSupport.readAsString(buffer));
        Assert.assertEquals(0, buffer.position());

        Assert.assertEquals("stream", JsonCodecSupport.readAsString(new ByteArrayInputStream("stream".getBytes(StandardCharsets.UTF_8))));
        Assert.assertEquals("reader", JsonCodecSupport.readAsString(new StringReader("reader")));
        Assert.assertEquals("clob", JsonCodecSupport.readAsString(new SerialClob("clob".toCharArray())));
        Assert.assertEquals("blob", JsonCodecSupport.readAsString(new SerialBlob("blob".getBytes(StandardCharsets.UTF_8))));
        Assert.assertEquals("123", JsonCodecSupport.readAsString(123));
    }

    private static Term term(Object value) {
        Term term = new Term();
        term.setColumn("metadata");
        term.setTermType("json_value");
        term.setValue(value);
        return term;
    }
}
