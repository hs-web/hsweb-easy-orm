package org.hswebframework.ezorm.rdb.codec;

import io.r2dbc.spi.Clob;
import org.junit.Test;
import reactor.core.publisher.Mono;

import static org.junit.Assert.*;

public class ClobValueCodecTest {


    @Test
    public void test() {
        ClobValueCodec codec = new ClobValueCodec();
        assertEquals(codec.decode(codec.encode(codec.encode("test"))), "test");

        assertEquals(codec.decode(1),1);

    }

    @Test
    public void testR2dbc() {
        ClobValueCodec codec = new ClobValueCodec();
        assertEquals(codec.decode(Clob.from(Mono.just("test"))),"test");


    }

}