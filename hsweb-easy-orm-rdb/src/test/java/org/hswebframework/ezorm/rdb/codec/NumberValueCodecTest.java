package org.hswebframework.ezorm.rdb.codec;

import org.hswebframework.utils.time.DateFormatter;
import org.joda.time.DateTime;
import org.junit.Assert;
import org.junit.Test;

import java.text.DecimalFormat;
import java.util.Date;

import static org.junit.Assert.*;

public class NumberValueCodecTest {

    @Test
    public void test() {
        {
            NumberValueCodec codec = new NumberValueCodec(Integer.class);
            Assert.assertNull(codec.encode(null));
            Assert.assertNull(codec.decode(null));
            Assert.assertEquals(codec.encode("1"), 1);
            Assert.assertEquals(codec.encode(true), 1);
            Assert.assertEquals(codec.encode(false), 0);
            Assert.assertEquals(codec.decode("1"), 1);
        }
        {
            NumberValueCodec codec = new NumberValueCodec(Long.class);
            Date date=new DateTime().withDate(2019,10,1)
                    .withMillisOfDay(0)
                    .toDate();
            String str = DateFormatter.toString(date, "yyyy-MM-dd");
            Assert.assertEquals(codec.encode(str), date.getTime());
            Assert.assertEquals(codec.encode(1), 1L);
            Assert.assertEquals(codec.decode(10L), 10L);

        }
        {
            NumberValueCodec codec = new NumberValueCodec(Byte.class);

            Assert.assertEquals(codec.encode("1"), (byte) 1);
            Assert.assertEquals(codec.decode("1"), (byte) 1);
        }

        {
            NumberValueCodec codec = new NumberValueCodec(Short.class);

            Assert.assertEquals(codec.encode("1"), (short) 1);
            Assert.assertEquals(codec.decode("1"), (short) 1);
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
        {
            DecimalFormat format=new DecimalFormat(",###");
            NumberValueCodec codec = new NumberValueCodec(format::format);

            Assert.assertEquals(codec.decode(100_000_000),"100,000,000");
            Assert.assertEquals(codec.encode(100_000_000),"100,000,000");

        }


    }

}