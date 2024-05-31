package org.hswebframework.ezorm.rdb.codec;

import org.junit.Assert;
import org.junit.Test;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.*;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class DateTimeCodecTest {

    @Test
    public void testDecodeString() {
        DateTimeCodec codec = new DateTimeCodec("yyyy:MM:dd", String.class);

        Date data = new Date();

        Object val = codec.encode(data);
        assertEquals(data, val);

        assertEquals(codec.decode(codec.encode("2019:01:01")), "2019:01:01");

    }


    @Test
    public void testCodeList() {
        DateTimeCodec codec = new DateTimeCodec("yyyy-MM-dd", String.class);

        String str = "2019-01-01,2019-01-31";

        Object encode = codec.encode(str);
        Assert.assertTrue(encode instanceof List);

        Assert.assertEquals(str,codec.decode(encode));
    }

    @Test
    public void testDeCodeList() {
        DateTimeCodec codec = new DateTimeCodec("yyyy-MM-dd", Date.class);

        String str = "2019-01-01,2019-01-31";

        Object encode = codec.decode(str);
        Assert.assertTrue(encode instanceof List);

        Assert.assertArrayEquals(((List) encode).toArray(), Arrays.stream(str.split("[,]")).map(codec::encode).toArray());
    }
    @Test
    public void testDecodeInstant() {
        DateTimeCodec codec = new DateTimeCodec("yyyy-MM-dd", Instant.class);

        Date data = new Date();

        Object val = codec.encode(data);
        assertEquals(data, val);

        Object date = codec.encode("2019-01-01");

      assertTrue(codec.decode(date) instanceof Instant);
    }
    @Test
    public void testDecodeDate() {
        DateTimeCodec codec = new DateTimeCodec("yyyy-MM-dd", Date.class);

        Date data = new Date();

        Object val = codec.encode(data);
        assertEquals(data, val);

        Object date = codec.encode("2019-01-01");

        assertEquals(codec.decode(date), date);

        assertEquals(codec.decode(((Date) date).getTime()), date);

    }

    @Test
    public void testDecodeNumberTime() {
        DateTimeCodec codec = new DateTimeCodec("HH:mm:ss", LocalTime.class);
        long time = System.currentTimeMillis();

        Object decode = codec.decode(time);
        LocalTime localTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(time), ZoneId.systemDefault()).toLocalTime();
        assertEquals(localTime, decode);
    }
}