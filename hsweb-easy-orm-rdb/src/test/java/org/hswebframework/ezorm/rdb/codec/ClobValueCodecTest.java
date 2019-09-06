package org.hswebframework.ezorm.rdb.codec;

import org.junit.Test;

import static org.junit.Assert.*;

public class ClobValueCodecTest {


    @Test
    public void test() {
        ClobValueCodec codec = new ClobValueCodec();
        assertEquals(codec.decode(codec.encode(codec.encode("test"))), "test");

        assertEquals(codec.decode(1),1);

    }

}