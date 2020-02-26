package org.hswebframework.ezorm.core.dsl;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.param.Term;
import org.junit.Assert;
import org.junit.Test;

import java.io.Serializable;
import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.function.Consumer;
import java.util.function.Function;

import static org.junit.Assert.*;

public class QueryTest {

    @Test
    public void testIncludeExclude() {

        Assert.assertTrue(Query.of().select("id").getParam().getIncludes().contains("id"));

        Assert.assertTrue(Query.of().excludes("id").getParam().getExcludes().contains("id"));

        Assert.assertTrue(Query.of().select(TestEntity::getName).getParam().getIncludes().contains("name"));

        Assert.assertTrue(Query.of().excludes(TestEntity::getName).getParam().getExcludes().contains("name"));

    }

    @Test
    public void testWhere() {
        {
            Term term = Query.of().where(TestEntity::getId, "123").getParam().getTerms().get(0);
            Assert.assertEquals(term.getColumn(), "id");
            Assert.assertEquals(term.getTermType(), "eq");
            Assert.assertEquals(term.getValue(), "123");
        }

        //like
        {
            assertSingleCondition(query -> query.like(TestEntity::getId, "123"), "like", "123");
            assertSingleCondition(query -> query.like$(TestEntity::getId, "123"), "like", "123%");
            assertSingleCondition(query -> query.$like(TestEntity::getId, "123"), "like", "%123");
            assertSingleCondition(query -> query.$like$(TestEntity::getId, "123"), "like", "%123%");
        }

        //in
        {
            assertSingleCondition(query -> query.in(TestEntity::getId, 1), "in", 1);
            assertSingleCondition(query -> query.in(TestEntity::getId, 1, 2, 3), "in", new Object[]{1, 2, 3});
            assertSingleCondition(query -> query.in(TestEntity::getId, Arrays.asList(1, 2, 3)), "in",  Arrays.asList(1, 2, 3));
        }

        //gt lt
        {
            assertSingleCondition(query -> query.gt(TestEntity::getId, 1), "gt", 1);
            assertSingleCondition(query -> query.gte(TestEntity::getId, 1), "gte",1);
            assertSingleCondition(query -> query.lt(TestEntity::getId,1), "lt", 1);
            assertSingleCondition(query -> query.lte(TestEntity::getId,1), "lte", 1);
        }



    }

    private void assertSingleCondition(Function<Conditional<?>, Conditional<?>> consumer, String termType, Object value) {
        Term term = ((Query)consumer.apply(Query.of())).getParam().getTerms().get(0);
        Assert.assertEquals(term.getTermType(), termType);

        if (value instanceof Object[]) {
            Assert.assertArrayEquals((Object[]) term.getValue(), (Object[]) value);
        } else {
            Assert.assertEquals(term.getValue(), value);
        }
    }


    @Getter
    @Setter
    static class TestEntity implements Serializable {
        String id;
        String name;
    }
}