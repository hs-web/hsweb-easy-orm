package org.hswebframework.ezorm.core.param;

import org.junit.Assert;
import org.junit.Test;

import static org.junit.Assert.*;

public class SortTest {

    @Test
    public void test(){
        Sort sort=new Sort();
        sort.setOrder("-- delete ");
        Assert.assertEquals(sort.getOrder(),"asc");
    }

}