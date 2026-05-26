package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.junit.Assert;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;

public class PostgresqlVectorDistanceFunctionFragmentBuilderBranchTest {
    @Test
    public void testCreateBranches() {
        PostgresqlVectorDistanceFunctionFragmentBuilder builder = new PostgresqlVectorDistanceFunctionFragmentBuilder();
        RDBColumnMetadata column = new H2SchemaMetadata("PUBLIC").newTable("v").newColumn(); column.setName("v");
        Assert.assertTrue(builder.create("t.v", column, Map.of("vectorValue", new float[]{1,2,3})).isNotEmpty());
        Map<String, Object> none = new HashMap<>();
        none.put("termType", VectorTermType.vector_ip.name());
        none.put("vectorValue", null);
        Assert.assertTrue(builder.create("t.v", column, none).isEmpty());
        Map<String, Object> empty = new HashMap<>();
        empty.put("termType", 1);
        empty.put("vectorValue", null);
        Assert.assertTrue(builder.create("t.v", column, empty).isEmpty());
        try {
            Map<String, Object> bad = new HashMap<>();
            bad.put("termType", "unknown");
            bad.put("vectorValue", new float[]{1,2,3});
            builder.create("t.v", column, bad);
            Assert.fail();
        } catch (IllegalArgumentException ignore) {
        }
    }
}
