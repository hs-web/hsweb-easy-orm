package org.hswebframework.ezorm.rdb.mapping.defaults.record;

import org.junit.Assert;
import org.junit.Test;

import java.util.*;

public class DefaultRecordBranchTest {

    @Test
    public void testTypedAccessAndNullSkipping() {
        Date now = new Date();
        Record record = Record.newRecord()
            .putValue("name", "JetLinks")
            .putValue("age", 18)
            .putValue("enabledNumber", 1)
            .putValue("enabledTrue", true)
            .putValue("enabledY", "Y")
            .putValue("enabledLowerY", "y")
            .putValue("enabledOne", "1")
            .putValue("created", now)
            .putValue("ignored", null);

        Assert.assertEquals("JetLinks", record.getString("name").orElse(null));
        Assert.assertEquals(Integer.valueOf(18), record.getInteger("age").orElse(null));
        Assert.assertTrue(record.getBoolean("enabledNumber").orElse(false));
        Assert.assertTrue(record.getBoolean("enabledTrue").orElse(false));
        Assert.assertTrue(record.getBoolean("enabledY").orElse(false));
        Assert.assertTrue(record.getBoolean("enabledLowerY").orElse(false));
        Assert.assertTrue(record.getBoolean("enabledOne").orElse(false));
        Assert.assertEquals(now, record.getDate("created").orElse(null));
        Assert.assertFalse(record.containsKey("ignored"));
        Assert.assertFalse(record.get("missing").isPresent());
    }

    @Test
    public void testNestAndNestsBranches() {
        Record child = Record.newRecord().putValue("name", "child");
        Map<String, Object> childMap = new LinkedHashMap<>();
        childMap.put("name", "mapChild");
        List<Record> children = Arrays.asList(child, Record.newRecord(childMap));

        Record record = Record.newRecord()
            .putValue("childRecord", child)
            .putValue("childMap", childMap)
            .putValue("children", children);

        Assert.assertSame(child, record.getNest("childRecord").orElseThrow());
        Assert.assertEquals("mapChild", record.getNest("childMap").orElseThrow().getString("name").orElse(null));
        Assert.assertEquals(1, record.getNests("childRecord").orElseThrow().size());
        Assert.assertEquals(1, record.getNests("childMap").orElseThrow().size());
        Assert.assertEquals(2, record.getNests("children").orElseThrow().size());
        Assert.assertEquals("child", record.apply(r -> r.getNest("childRecord").orElseThrow().getString("name").orElse(null)));
        final boolean[] accepted = {false};
        record.accept(r -> accepted[0] = r.containsKey("children"));
        Assert.assertTrue(accepted[0]);
    }

    @Test(expected = UnsupportedOperationException.class)
    public void testInvalidNestFailsFast() {
        Record.newRecord().putValue("bad", "text").getNest("bad");
    }

    @Test(expected = UnsupportedOperationException.class)
    public void testInvalidNestsFailsFast() {
        Record.newRecord().putValue("bad", "text").getNests("bad");
    }
}
