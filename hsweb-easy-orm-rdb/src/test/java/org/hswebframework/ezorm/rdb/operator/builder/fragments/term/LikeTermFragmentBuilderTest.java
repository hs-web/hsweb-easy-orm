package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;
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
            assertEquals("'123' like concat( '%' , test )",sql);
        }

        {
            String sql =  builder.createFragments("test", null, Terms.Like.reversal("test","123",false,true))
                                 .toRequest()
                                 .toNativeSql();
            assertEquals("'123' like concat( test , '%' )",sql);
        }

        {
            String sql =  builder.createFragments("test", null, Terms.Like.reversal("test","123",true,true))
                                 .toRequest()
                                 .toNativeSql();
            assertEquals("'123' like concat( concat( '%' , test ) , '%' )",sql);
        }

        {
            String sql = builder.createFragments("test", null, Terms.Like.reversal("test", "123", false, false))
                               .toRequest()
                               .toNativeSql();
            assertEquals("'123' like test", sql);
        }
    }

    @Test
    public void testIgnoreCase() {
        LikeTermFragmentBuilder builder = new LikeTermFragmentBuilder(false);
        Term term = Term.of("test", TermType.like + "$ignoreCase", "AbC");

        String sql = builder.createFragments("test", null, term)
                            .toRequest()
                            .toNativeSql();
        assertEquals("lower( test ) like lower( 'AbC' )", sql);
    }

    @Test
    public void testIgnoreCaseWithReversal() {
        LikeTermFragmentBuilder builder = new LikeTermFragmentBuilder(false);
        Term term = Terms.Like.reversal("test", "AbC", true, true);
        term.getOptions().add("ignoreCase");

        String sql = builder.createFragments("test", null, term)
                            .toRequest()
                            .toNativeSql();
        assertEquals("lower( 'AbC' ) like lower( concat( concat( '%' , test ) , '%' ) )", sql);
    }

    @Test
    public void testNotLikeIgnoreCase() {
        LikeTermFragmentBuilder builder = new LikeTermFragmentBuilder(true);
        Term term = Term.of("test", TermType.nlike + "$ignoreCase", "AbC");

        String sql = builder.createFragments("test", null, term)
                            .toRequest()
                            .toNativeSql();
        assertEquals("lower( test ) not like lower( 'AbC' )", sql);
    }

}
