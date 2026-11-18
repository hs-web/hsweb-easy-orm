package org.hswebframework.ezorm.rdb.codec;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.Test;

import java.math.BigDecimal;
import java.sql.JDBCType;
import java.util.Collections;
import java.util.Map;

import static org.junit.Assert.*;

public class CodecConfigurationTest {

    enum State { first, second }

    @Test
    public void standardConfigurationDescribesInstalledBehavior() {
        DateTimeCodec date = new DateTimeCodec("yyyyMMdd", String.class);
        assertEquals("yyyyMMdd", date.getFormat());
        assertSame(String.class, date.getToType());
        assertEquals("19700101", date.decode(date.encode("19700101")));

        BooleanValueCodec bool = new BooleanValueCodec(JDBCType.INTEGER);
        assertSame(JDBCType.INTEGER, bool.getSqlType());
        assertEquals(1, bool.encode(true));

        EnumValueCodec enumeration = new EnumValueCodec(State[].class, "ordinal", true);
        assertSame(State.class, enumeration.getType());
        assertEquals("ordinal", enumeration.getProperty());
        assertTrue(enumeration.isArray());
        assertTrue(enumeration.isToMask());
    }

    @Test
    public void arbitraryNumberFunctionHasNoReconstructableTarget() {
        NumberValueCodec standard = new NumberValueCodec(BigDecimal.class);
        assertSame(BigDecimal.class, standard.getJavaType());
        assertEquals(new BigDecimal("12.25"), standard.decode("12.25"));
        NumberValueCodec function = new NumberValueCodec(number -> number.doubleValue() / 100);
        assertNull(function.getJavaType());
        assertEquals(0.12D, function.decode(12));
    }

    @Test
    public void jsonConfigurationRetainsGenericTypeAndEffectiveOptions() {
        boolean sharedDecimals = JsonValueCodec.defaultMapper.isEnabled(DeserializationFeature.USE_BIG_DECIMAL_FOR_FLOATS);
        JsonInclude.Value sharedInclusion = JsonValueCodec.defaultMapper.getSerializationConfig().getDefaultPropertyInclusion();
        JsonValueCodec codec = JsonValueCodec.ofMap(Map.class, String.class, BigDecimal.class)
            .includeNulls(true).useBigDecimalForFloats(true);
        assertSame(Map.class, codec.getTargetType());
        assertSame(BigDecimal.class, codec.getJacksonType().getContentType().getRawClass());
        assertTrue(codec.isIncludeNulls());
        assertTrue(codec.isUseBigDecimalForFloats());
        assertFalse(codec.isCustomMapper());
        assertEquals("{\"empty\":null}", codec.encode(Collections.singletonMap("empty", null)));
        assertEquals(BigDecimal.ONE, ((Map<?, ?>) codec.decode("{\"amount\":1}")).get("amount"));
        assertEquals(sharedDecimals, JsonValueCodec.defaultMapper.isEnabled(DeserializationFeature.USE_BIG_DECIMAL_FOR_FLOATS));
        assertEquals(sharedInclusion, JsonValueCodec.defaultMapper.getSerializationConfig().getDefaultPropertyInclusion());
    }

    @Test
    public void standardOptionsDoNotEraseCustomMapperProvenance() {
        JsonValueCodec codec = JsonValueCodec.of(Map.class);
        codec.setMapper(new ObjectMapper());
        codec.includeNulls(true).useBigDecimalForFloats(true);
        assertTrue(codec.isCustomMapper());
        codec.setMapper(JsonValueCodec.defaultMapper);
        assertFalse(codec.isCustomMapper());
        assertEquals(JsonValueCodec.defaultMapper.isEnabled(DeserializationFeature.USE_BIG_DECIMAL_FOR_FLOATS),
            codec.isUseBigDecimalForFloats());
    }
}
