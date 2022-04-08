package org.hswebframework.ezorm.rdb.executor;

import org.junit.Test;

import java.util.Collections;

import static org.junit.Assert.*;

public class SqlRequestsTest {

    @Test
    public void templateTest() {
        SqlRequest request = SqlRequests.of("select * from order where id = #{id}", Collections.singletonMap("id", "o_id"));
        System.out.println(request);
        assertEquals(request.getSql(),"select * from order where id = ?");
        assertArrayEquals(request.getParameters(),new Object[]{"o_id"});
    }
}