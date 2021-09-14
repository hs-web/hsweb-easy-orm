package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.operator.dml.Terms;
import org.junit.Test;

import static org.junit.Assert.*;

public class LikeTermFragmentBuilderTest {

    @Test
    public void test(){
        LikeTermFragmentBuilder builder=new LikeTermFragmentBuilder(false);

        {
            String sql =  builder.createFragments("test", null, Terms.Like.of("test","123"))
                                 .toRequest().toNativeSql();
            assertEquals("test like '123'",sql);
        }
        {
            String sql =  builder.createFragments("test", null, Terms.Like.reversal("test","123",true,false))
                                 .toRequest()
                                 .toNativeSql();
            assertEquals("'123' like concat( '%', test )",sql);
        }

        {
            String sql =  builder.createFragments("test", null, Terms.Like.reversal("test","123",false,true))
                                 .toRequest()
                                 .toNativeSql();
            assertEquals("'123' like concat( test ,'%' )",sql);
        }

        {
            String sql =  builder.createFragments("test", null, Terms.Like.reversal("test","123",true,true))
                                 .toRequest()
                                 .toNativeSql();
            assertEquals("'123' like concat( '%', test ,'%' )",sql);
        }


    }
}