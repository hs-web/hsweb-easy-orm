package org.hswebframework.ezorm.rdb.codec;

import org.junit.Assert;
import org.junit.Test;

import java.io.NotSerializableException;
import java.sql.Blob;
import java.sql.Date;

import static org.junit.Assert.*;

public class BlobValueCodecTest {


    @Test
    public void testSimple() {
        BlobValueCodec codec = new BlobValueCodec();

        assertNull(codec.decode(null));
        assertNull(codec.encode(null));


        Object[] values = {"1", 1, true, 1L, 0.2D};

        for (Object value : values) {
            Object encode = codec.encode(value);
            assertNotNull(encode);
            assertTrue(encode instanceof Blob);

            Object decode = codec.decode(encode);

            assertEquals(decode, value);
        }
    }

    @Test
    public void testByteArr() {
        byte[] arr = {1, 2, 3};
        BlobValueCodec codec = new BlobValueCodec();

        assertArrayEquals(arr, (byte[]) codec.decode(codec.encode(arr)));

    }

    @Test
    public void testError() {
        BlobValueCodec codec = new BlobValueCodec();

        try {
            codec.encode(new Object());
            fail();
        } catch (Exception e) {
            assertTrue(e instanceof NotSerializableException);
        }

    }


}