package org.hswebframework.ezorm.rdb.executor;

import org.junit.Test;

import static org.junit.Assert.*;

public class SqlTemplateParserTest {


    @Test
    public void testParse() {

        SqlRequest request = SqlTemplateParser.parse("select * from user where name = #{name} and status=1", (__) -> "1234");

        assertNotNull(request);
        assertEquals(request.getSql(),"select * from user where name = ? and status=1");
        assertArrayEquals(request.getParameters(),new Object[]{"1234"} );

        assertTrue(request instanceof PrepareSqlRequest);

        assertEquals(((PrepareSqlRequest) request).toNativeSql(),"select * from user where name = '1234' and status=1");


    }
}