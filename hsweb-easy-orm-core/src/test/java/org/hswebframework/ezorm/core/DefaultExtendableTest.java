package org.hswebframework.ezorm.core;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.SneakyThrows;
import org.junit.Test;

import static org.junit.Assert.*;

public class DefaultExtendableTest {


    @Test
    @SneakyThrows
    public void testJson() {
        DefaultExtendable entity = new DefaultExtendable();

        entity.setExtension("extName", "test");

        ObjectMapper mapper = new ObjectMapper();

        String json = mapper.writerFor(DefaultExtendable.class).writeValueAsString(entity);

        System.out.println(json);
        DefaultExtendable decoded = mapper.readerFor(DefaultExtendable.class).readValue(json);

        assertNotNull(decoded.getExtension("extName"));

        assertEquals(entity.getExtension("extName"), decoded.getExtension("extName"));
    }

}