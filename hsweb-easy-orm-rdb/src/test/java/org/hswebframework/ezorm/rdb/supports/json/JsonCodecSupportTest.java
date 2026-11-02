package org.hswebframework.ezorm.rdb.supports.json;

import org.junit.Assert;
import org.junit.Test;
import org.postgresql.util.PGobject;

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

    @Test
    public void testJdbcPostgresqlJsonReader() throws Exception {
        PGobject pgObject = new PGobject();
        pgObject.setType("jsonb");
        pgObject.setValue("{\"id\":\"test\"}");

        JsonStringReader reader = new JdbcPostgresqlJsonStringReader();
        Assert.assertTrue(reader.supports(pgObject));
        Assert.assertEquals("{\"id\":\"test\"}", reader.read(pgObject));
        Assert.assertFalse(reader.supports(new SamplePgObject("{\"id\":\"test\"}")));
        Assert.assertNull(reader.read(new SamplePgObject("{\"id\":\"test\"}")));
    }

    private static class SampleJson {
        private final String value;

        private SampleJson(String value) {
            this.value = value;
        }
    }

    private static class SamplePgObject extends PGobject {

        private SamplePgObject(String value) throws Exception {
            setValue(value);
        }
    }
}
