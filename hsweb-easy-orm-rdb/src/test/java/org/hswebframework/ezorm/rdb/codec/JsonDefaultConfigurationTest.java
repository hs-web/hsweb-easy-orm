package org.hswebframework.ezorm.rdb.codec;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.SerializationFeature;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.beanutils.PropertyUtils;
import org.hswebframework.ezorm.rdb.mapping.annotation.JsonCodec;
import org.hswebframework.ezorm.rdb.mapping.jpa.SimpleEntityPropertyDescriptor;
import org.hswebframework.ezorm.rdb.mapping.parser.DefaultValueCodecResolver;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.supports.json.JsonCodecSupport;
import org.junit.Test;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

public class JsonDefaultConfigurationTest {
    @Test
    public void annotationCodecRetainsLegacyDefaultConfiguration() {
        var mapper = JsonValueCodec.defaultMapper;
        boolean original = mapper.isEnabled(DeserializationFeature.ACCEPT_SINGLE_VALUE_AS_ARRAY);
        try {
            mapper.enable(DeserializationFeature.ACCEPT_SINGLE_VALUE_AS_ARRAY);
            var property = PropertyUtils.getPropertyDescriptors(Holder.class);
            var descriptor = java.util.Arrays.stream(property).filter(p -> p.getName().equals("items"))
                .findFirst().orElseThrow();
            var codec = DefaultValueCodecResolver.COMMONS
                .resolve(SimpleEntityPropertyDescriptor.of(descriptor, null)).orElseThrow();
            assertEquals(List.of("one"), codec.decode("\"one\""));
        } finally {
            mapper.configure(DeserializationFeature.ACCEPT_SINGLE_VALUE_AS_ARRAY, original);
        }
    }

    @Test
    public void sqlParameterCodecsRetainLegacyDefaultConfiguration() {
        var mapper = JsonValueCodec.defaultMapper;
        boolean original = mapper.isEnabled(SerializationFeature.INDENT_OUTPUT);
        try {
            mapper.enable(SerializationFeature.INDENT_OUTPUT);
            Map<String, Integer> value = Map.of("number", 1);
            String expected = JsonCodecSupport.toJson(value);
            assertTrue(expected.contains("\n"));
            assertEquals(expected, org.hswebframework.ezorm.rdb.supports.json.JsonType.INSTANCE.encode(value));
            assertEquals(expected, ((NativeSql) org.hswebframework.ezorm.rdb.supports.postgres.JsonType.INSTANCE
                .encode(value)).getParameters()[0]);
            assertEquals(expected, ((NativeSql) org.hswebframework.ezorm.rdb.supports.postgres.JsonbType.INSTANCE
                .encode(value)).getParameters()[0]);
        } finally {
            mapper.configure(SerializationFeature.INDENT_OUTPUT, original);
        }
    }

    @Getter
    @Setter
    public static class Holder {
        @JsonCodec
        private List<String> items;
    }
}
