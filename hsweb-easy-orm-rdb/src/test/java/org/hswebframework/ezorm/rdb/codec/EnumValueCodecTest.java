package org.hswebframework.ezorm.rdb.codec;

import org.junit.Assert;
import org.junit.Test;

import static org.junit.Assert.*;

public class EnumValueCodecTest {


    @Test
    public void testSimple() {

        EnumValueCodec codec = new EnumValueCodec(TestEnum.class);

        Assert.assertEquals("A", codec.encode(TestEnum.A));

        Assert.assertEquals(TestEnum.A, codec.decode("A"));

    }

    @Test
    public void testArray() {

        EnumValueCodec codec = new EnumValueCodec(TestEnum[].class);

        Assert.assertEquals("A,C", codec.encode(new TestEnum[]{TestEnum.A, TestEnum.C}));

        Assert.assertArrayEquals(new TestEnum[]{TestEnum.A}, (Object[]) codec.decode("A"));

    }

    @Test
    public void testMask() {

        EnumValueCodec codec = new EnumValueCodec(TestEnum.class, true);

        Assert.assertEquals(1L, codec.encode(TestEnum.A));

        Assert.assertEquals(TestEnum.A, codec.decode(1));

    }

    @Test
    public void testMaskArray() {

        EnumValueCodec codec = new EnumValueCodec(TestEnum[].class, true);

        Assert.assertEquals(3L, codec.encode(new TestEnum[]{TestEnum.A, TestEnum.B}));

        Assert.assertArrayEquals(new TestEnum[]{TestEnum.A, TestEnum.B}, (Object[]) codec.decode(3));

    }

    @Test
    public void testCustomProperty() {
        EnumValueCodec codec = new EnumValueCodec(TestEnum.class, "property",false);

        for (TestEnum value : TestEnum.values()) {
            assertEquals(value, codec.decode(Long.valueOf(value.property).intValue()));
        }

    }

    public enum TestEnum {
        A, B, C;

        final long property  = ordinal() * 10;

        public long getProperty() {
            return property;
        }
    }


}