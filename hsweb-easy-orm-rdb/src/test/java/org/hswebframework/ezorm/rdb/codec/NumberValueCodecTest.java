package org.hswebframework.ezorm.rdb.codec;

import org.junit.Assert;
import org.junit.Test;

import java.util.Date;

import static org.junit.Assert.*;

public class NumberValueCodecTest {

    @Test
    public void test() {
        {
            NumberValueCodec codec = new NumberValueCodec(Integer.class);

            Assert.assertEquals(codec.encode("1"), 1);
            Assert.assertEquals(codec.decode("1"), 1);
        }

        {
            NumberValueCodec codec = new NumberValueCodec(Byte.class);

            Assert.assertEquals(codec.encode("1"), (byte) 1);
            Assert.assertEquals(codec.decode("1"), (byte) 1);
        }

        {
            NumberValueCodec codec = new NumberValueCodec(Double.class);

            Assert.assertEquals(codec.encode("1.999"), 1.999D);
            Assert.assertEquals(codec.decode("2.0"), 2D);
        }

        {
            NumberValueCodec codec = new NumberValueCodec(Float.class);

            Assert.assertEquals(codec.encode("1.999"), 1.999F);
            Assert.assertEquals(codec.decode("2.0"), 2F);
        }

        {
            NumberValueCodec codec = new NumberValueCodec(Date.class);
            long time = System.currentTimeMillis();

            Assert.assertEquals(codec.encode(new Date(time)), time);
            Assert.assertEquals(codec.decode(time), new Date(time));
        }


    }

}