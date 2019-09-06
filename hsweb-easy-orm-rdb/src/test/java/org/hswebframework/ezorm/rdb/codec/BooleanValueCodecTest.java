package org.hswebframework.ezorm.rdb.codec;

import org.junit.Assert;
import org.junit.Test;

import java.sql.JDBCType;

import static org.junit.Assert.*;

public class BooleanValueCodecTest {


    @Test
    public void testNumeric(){
        BooleanValueCodec codec=new BooleanValueCodec(JDBCType.NUMERIC);

        Assert.assertEquals(codec.encode(true),1);
        Assert.assertEquals(codec.encode(false),0);

        Assert.assertEquals(codec.encode("true"),1);
        Assert.assertEquals(codec.encode("1"),1);
        Assert.assertEquals(codec.encode("false"),0);


        Assert.assertEquals(codec.decode(1),true);
        Assert.assertEquals(codec.decode(0),false);
        Assert.assertEquals(codec.decode("1"),true);
        Assert.assertEquals(codec.decode("0"),false);
        Assert.assertEquals(codec.decode("true"),true);
        Assert.assertEquals(codec.decode("false"),false);

        Assert.assertEquals(codec.decode(true),true);
        Assert.assertEquals(codec.decode(false),false);
    }

    @Test
    public void testBoolean(){
        BooleanValueCodec codec=new BooleanValueCodec(JDBCType.BOOLEAN);

        Assert.assertEquals(codec.encode(true),true);
        Assert.assertEquals(codec.encode(false),false);
        Assert.assertEquals(codec.encode("true"),true);
        Assert.assertEquals(codec.encode("1"),true);
        Assert.assertEquals(codec.encode("false"),false);

        Assert.assertEquals(codec.decode(1),true);
        Assert.assertEquals(codec.decode(0),false);
        Assert.assertEquals(codec.decode("1"),true);
        Assert.assertEquals(codec.decode("0"),false);
        Assert.assertEquals(codec.decode("true"),true);
        Assert.assertEquals(codec.decode("false"),false);

        Assert.assertEquals(codec.decode(true),true);
        Assert.assertEquals(codec.decode(false),false);
    }

}