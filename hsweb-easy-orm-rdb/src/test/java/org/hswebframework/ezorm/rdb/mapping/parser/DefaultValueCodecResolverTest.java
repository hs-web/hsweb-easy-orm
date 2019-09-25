package org.hswebframework.ezorm.rdb.mapping.parser;

import lombok.Getter;
import lombok.Setter;
import org.apache.commons.beanutils.PropertyUtils;
import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.rdb.codec.DateTimeCodec;
import org.hswebframework.ezorm.rdb.codec.JsonValueCodec;
import org.hswebframework.ezorm.rdb.mapping.annotation.JsonCodec;
import org.junit.Assert;
import org.junit.Test;

import java.beans.PropertyDescriptor;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class DefaultValueCodecResolverTest {


    @Test
    public void test(){
        DefaultValueCodecResolver resolver=DefaultValueCodecResolver.COMMONS;

        Map<String, PropertyDescriptor> descriptorMap = Stream.of(PropertyUtils.getPropertyDescriptors(Entity.class))
                .collect(Collectors.toMap(PropertyDescriptor::getName, Function.identity()));


        {
          ValueCodec codec= resolver.resolve(Entity.class,descriptorMap.get("date"))
                    .orElseThrow(NullPointerException::new);

            Assert.assertTrue(codec instanceof DateTimeCodec);
        }

        {
            ValueCodec codec= resolver.resolve(Entity.class,descriptorMap.get("arr"))
                    .orElseThrow(NullPointerException::new);

            Assert.assertTrue(codec instanceof JsonValueCodec);
        }

    }

    @Getter
    @Setter
    public static class Entity {

        private String name;

        private Date date;

        @JsonCodec
        private List<String> arr;

    }
}