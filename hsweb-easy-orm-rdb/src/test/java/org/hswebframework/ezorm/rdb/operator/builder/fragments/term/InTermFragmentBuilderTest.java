package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.junit.Assert;
import org.junit.Test;
import reactor.core.publisher.Flux;

import java.util.List;
import java.util.stream.Stream;

import static org.junit.Assert.*;

public class InTermFragmentBuilderTest {

    @Test
    public void test() {

        InTermFragmentBuilder builder = new InTermFragmentBuilder("in", "在...之中",false);

        Term term = new Term();
        term.setValue("1");

        SqlFragments fragments = builder.createFragments("id", null, term);

        assertNotNull(fragments);
        SqlRequest request = fragments.toRequest();

        System.out.println(request.getSql());
        Assert.assertEquals(request.getSql(), "id in( ? )");

    }

    @Test
    public void testLarge() {

        InTermFragmentBuilder builder = new InTermFragmentBuilder("in", "在...之中",false);

        Term term = new Term();
        List<Integer> values = Flux.range(0, 510).collectList().block();
        assertNotNull(values);

        term.setValue(values);

        SqlFragments fragments = builder.createFragments("id", null, term);

        assertNotNull(fragments);
        SqlRequest request = fragments.toRequest();

        assertArrayEquals(values.toArray(),request.getParameters());

        System.out.println(request.getSql());
        // Assert.assertEquals(request.getSql(), "id in( ? )");

    }

}