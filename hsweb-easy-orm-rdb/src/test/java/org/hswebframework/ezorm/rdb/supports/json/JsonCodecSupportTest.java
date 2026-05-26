package org.hswebframework.ezorm.rdb.supports.json;

import org.junit.Assert;
import org.junit.Test;

public class JsonCodecSupportTest {

    @Test
    public void testReadAsStringWithoutReaders() {
        Assert.assertEquals("abc", JsonCodecSupport.readAsString("abc"));
        Assert.assertTrue(JsonCodecSupport.canReadAsString("abc"));
    }

    @Test
    public void testRegisterStringReader() {
        JsonStringReader reader = new JsonStringReader() {
            @Override
            public boolean supports(Object data) {
                return data instanceof SampleJson;
            }

            @Override
            public String read(Object data) {
                return ((SampleJson) data).value;
            }
        };
        JsonCodecSupport.registerStringReader(reader);

        Assert.assertTrue(JsonCodecSupport.canReadAsString(new SampleJson("ok")));
        Assert.assertEquals("ok", JsonCodecSupport.readAsString(new SampleJson("ok")));
    }

    private static class SampleJson {
        private final String value;

        private SampleJson(String value) {
            this.value = value;
        }
    }
}
