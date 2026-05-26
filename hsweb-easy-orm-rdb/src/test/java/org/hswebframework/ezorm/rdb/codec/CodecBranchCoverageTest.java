package org.hswebframework.ezorm.rdb.codec;

import org.hswebframework.ezorm.core.GlobalConfig;
import org.hswebframework.ezorm.core.ObjectPropertyOperator;
import org.hswebframework.ezorm.core.ApacheCommonPropertyOperator;
import org.hswebframework.ezorm.rdb.supports.json.JsonCodecSupport;
import org.junit.Assert;
import org.junit.Test;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;

public class CodecBranchCoverageTest {

    enum SampleEnum {
        A, B
    }

    enum NamedEnum {
        ONE("one"), TWO("two");

        private final String code;

        NamedEnum(String code) {
            this.code = code;
        }

        public String getCode() {
            return code;
        }
    }

    @Test
    public void testNumberCodecEncodeDecodeBranches() {
        NumberValueCodec longCodec = new NumberValueCodec(Long.class);
        Assert.assertNull(longCodec.encode(null));
        Assert.assertEquals(12L, longCodec.encode("12"));
        Assert.assertEquals(12L, longCodec.decode("12"));
        Assert.assertEquals(1L, longCodec.encode(true));
        Assert.assertEquals(0L, longCodec.encode(false));
        Assert.assertEquals(10L, longCodec.encode(new BigDecimal("10")));
        Assert.assertEquals(11L, longCodec.encode(new BigInteger("11")));
        Assert.assertNotNull(longCodec.encode(new java.util.Date(1000)));
        Assert.assertNotNull(longCodec.encode(java.time.LocalDateTime.of(2026, 1, 1, 0, 0)));
        Assert.assertNotNull(longCodec.encode(java.time.ZonedDateTime.of(2026, 1, 1, 0, 0, 0, 0, java.time.ZoneId.systemDefault())));
        Assert.assertEquals(1L, longCodec.decode(true));
        try {
            longCodec.encode("bad-number");
            Assert.fail("expected invalid number to fail fast");
        } catch (Exception ignore) {
        }
    }

    @Test
    public void testEnumCodecMaskAndPropertyBranches() {
        EnumValueCodec nameCodec = new EnumValueCodec(SampleEnum.class);
        Assert.assertEquals("A", nameCodec.encode(SampleEnum.A));
        Assert.assertEquals(SampleEnum.B, nameCodec.decode("B"));
        Assert.assertEquals("A,B", nameCodec.encode(new SampleEnum[]{SampleEnum.A, SampleEnum.B}));

        EnumValueCodec maskCodec = new EnumValueCodec(SampleEnum.class, true);
        Assert.assertEquals(3L, maskCodec.encode(new SampleEnum[]{SampleEnum.A, SampleEnum.B}));
        Assert.assertEquals(SampleEnum.A, maskCodec.decode(1));
        Assert.assertEquals(SampleEnum.B, maskCodec.decode(2));
        Assert.assertEquals(SampleEnum.A, maskCodec.decode("A"));

        EnumValueCodec propCodec = new EnumValueCodec(NamedEnum.class, "code");
        GlobalConfig.setPropertyOperator(new ObjectPropertyOperator() {
            @Override
            public java.util.Optional<Object> getProperty(Object object, String name) {
                return "code".equals(name) ? java.util.Optional.of(((NamedEnum) object).getCode()) : java.util.Optional.empty();
            }

            @Override
            public void setProperty(Object object, String name, Object value) {
            }
        });
        try {
            Assert.assertEquals("one", propCodec.encode(NamedEnum.ONE));
            Assert.assertEquals(NamedEnum.TWO, propCodec.decode("two"));
        } finally {
            GlobalConfig.setPropertyOperator(ApacheCommonPropertyOperator.INSTANCE);
        }

        EnumValueCodec arrayMaskCodec = new EnumValueCodec(SampleEnum[].class, true);
        Assert.assertTrue(arrayMaskCodec.decode(3L) instanceof SampleEnum[]);
    }

    @Test
    public void testJsonCodecSupportArrayAndReaderBranches() throws Exception {
        Assert.assertEquals("[1,2]", JsonCodecSupport.toJson(Arrays.asList(1, 2)));
        Assert.assertTrue(JsonCodecSupport.canReadAsString(new StringBuilder("x")));
        Assert.assertEquals("x", JsonCodecSupport.readAsString(new StringBuilder("x")));
        Assert.assertEquals("abc", JsonCodecSupport.readAsString(new java.io.StringReader("abc")));
    }
}
