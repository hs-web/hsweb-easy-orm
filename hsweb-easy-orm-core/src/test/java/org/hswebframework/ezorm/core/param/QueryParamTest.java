package org.hswebframework.ezorm.core.param;

import org.junit.Assert;
import org.junit.Test;

import static org.junit.Assert.*;

public class QueryParamTest {

    @Test
    public void testPage() {
        QueryParam param = new QueryParam();

        param.doPaging(2, 10);

        param.rePaging(15);//总数只有15条,重新分页到最后一页

        Assert.assertEquals(param.getPageIndex(), 1);

    }

    @Test
    public void testCustomFirstPage(){
        QueryParam param = new QueryParam();
        param.setFirstPageIndex(1);

        param.doPaging(1,10);

        Assert.assertEquals(param.getPageIndex(),0);
        Assert.assertEquals(param.getThinkPageIndex(),1);

        param.doPaging(2,10);

        Assert.assertEquals(param.getPageIndex(),1);

        Assert.assertEquals(param.getThinkPageIndex(),2);

    }

    @Test
    public void testCustomFirstPage2(){
        QueryParam param = new QueryParam();

        param.doPaging(4,10);

        param.setFirstPageIndex(1);

        Assert.assertEquals(param.getPageIndex(),3);
        Assert.assertEquals(param.getThinkPageIndex(),4);

        param.setPageIndex(10);
        Assert.assertEquals(param.getPageIndex(),9);
        Assert.assertEquals(param.getThinkPageIndex(),10);

        Assert.assertEquals(param.getFirstPageIndex(),1);

        param.setFirstPageIndex(0);

        Assert.assertEquals(param.getPageIndex(),8);
        Assert.assertEquals(param.getThinkPageIndex(),8);

    }
}