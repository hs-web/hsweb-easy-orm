package org.hswebframework.ezorm.core;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.SneakyThrows;
import org.junit.Test;

import static org.junit.Assert.*;

public class DefaultExtensibleTest {


    @Test
    @SneakyThrows
    public void testJson() {
        DefaultExtensible entity = new DefaultExtensible();

        entity.setExtension("extName", "test");

        ObjectMapper mapper = new ObjectMapper();

        String json = mapper.writerFor(DefaultExtensible.class).writeValueAsString(entity);

        System.out.println(json);
        DefaultExtensible decoded = mapper.readerFor(DefaultExtensible.class).readValue(json);

        assertNotNull(decoded.getExtension("extName"));

        assertEquals(entity.getExtension("extName"), decoded.getExtension("extName"));
    }

}