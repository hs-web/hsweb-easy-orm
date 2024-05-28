package org.hswebframework.ezorm.rdb.utils;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.junit.Test;

import static org.junit.Assert.*;

public class SqlUtilsTest {

    @Test
    public void testCreateQuestionMarks(){

        {
            SqlFragments fragments = SqlUtils.createQuestionMarks(200);
            assertNotNull(fragments);
            System.out.println(fragments.toRequest().getSql());
            assertEquals(200,fragments.toRequest().getSql().split(",").length);
        }

        {
            SqlFragments fragments = SqlUtils.createQuestionMarks(512);
            assertNotNull(fragments);
            System.out.println(fragments.toRequest().getSql());
            assertEquals(512,fragments.toRequest().getSql().split(",").length);
        }

        {
            SqlFragments fragments = SqlUtils.createQuestionMarks(1024);
            assertNotNull(fragments);
            System.out.println(fragments.toRequest().getSql());
            assertEquals(1024,fragments.toRequest().getSql().split(",").length);
        }
    }

}