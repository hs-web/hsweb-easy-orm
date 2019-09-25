package org.hswebframework.ezorm.rdb.codec;

import lombok.Getter;
import lombok.Setter;
import lombok.SneakyThrows;
import org.junit.Assert;
import org.junit.Test;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.Assert.*;

public class JsonValueCodecTest {


    @Test
    public void testList() {
        JsonValueCodec codec = JsonValueCodec.ofCollection(List.class, String.class);

        Object arr = codec.decode("[\"1\",\"2\"]");

        Assert.assertTrue(arr instanceof List);

        Assert.assertEquals(((List) arr).size(), 2);

    }

    @Test
    public void testSet() {
        JsonValueCodec codec = JsonValueCodec.ofCollection(Set.class, String.class);

        Object arr = codec.decode("[\"1\",\"2\"]");

        Assert.assertTrue(arr instanceof Set);

        Assert.assertEquals(((Set) arr).size(), 2);

    }


    @Test
    public void testMap() {
        JsonValueCodec codec = JsonValueCodec.ofMap(Map.class, String.class,Integer.class);

        Object arr = codec.decode("{\"a\":1,\"b\":\"2\"}");

        Assert.assertTrue(arr instanceof Map);

        Map<String,Integer> val = ((Map) arr);

        Assert.assertEquals(val.size(), 2);
        Assert.assertEquals(val.get("a"), Integer.valueOf(1));
        Assert.assertEquals(val.get("b"), Integer.valueOf(2));
    }

    @Test
    public void testEntity() {
        JsonValueCodec codec = JsonValueCodec.of(JsonCodecEntity.class);


        Object val = codec.decode("{\"name\":\"test\",\"time\":\"2019-10-01\"}");

        Assert.assertTrue(val instanceof JsonCodecEntity);

        JsonCodecEntity entity = ((JsonCodecEntity) val);

        Assert.assertEquals(entity.name, "test");
        Assert.assertNotNull(entity.time);
    }

    @Test
    @SneakyThrows
    public void testField() {
        JsonValueCodec codec = JsonValueCodec.ofField(JsonCodecEntity.class.getDeclaredField("nest"));


        Object val = codec.decode("{\"nest\":{\"name\":\"test\"}}");

        Assert.assertTrue(val instanceof Map);

        JsonCodecEntity entity = (JsonCodecEntity)((Map) val).get("nest");

        Assert.assertEquals(entity.name, "test");
    }

    @Test
    @SneakyThrows
    public void testField2() {
        JsonValueCodec codec = JsonValueCodec.ofField(JsonCodecEntity.class.getDeclaredField("nest2"));


        Object val = codec.decode("[{\"name\":\"test\"}]");

        Assert.assertTrue(val instanceof List);

        JsonCodecEntity entity = (JsonCodecEntity)((List) val).get(0);

        Assert.assertEquals(entity.name, "test");
    }


    @Getter
    @Setter
    public static class JsonCodecEntity{
        private String name;

        private Date time;

        private Map<String,JsonCodecEntity> nest;

        private List<JsonCodecEntity> nest2;

    }

}