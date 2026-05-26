package org.hswebframework.ezorm.rdb.codec;

import org.junit.Assert;
import org.junit.Test;

public class CompositeValueCodecTest {

    @Test
    public void testEncodeAndDecodePipelineOrder() {
        CompositeValueCodec codec = new CompositeValueCodec()
            .addEncoder(value -> value + "-second")
            .addEncoderFirst(value -> "first-" + value)
            .addDecoder(value -> value + "-decoded2")
            .addDecoderFirst(value -> "decoded1-" + value);

        Assert.assertEquals("first-value-second", codec.encode("value"));
        Assert.assertEquals("decoded1-value-decoded2", codec.decode("value"));
    }

    @Test
    public void testEmptyPipelineKeepsValue() {
        CompositeValueCodec codec = new CompositeValueCodec();
        Assert.assertEquals("value", codec.encode("value"));
        Assert.assertEquals("value", codec.decode("value"));
    }
}
