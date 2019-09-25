package org.hswebframework.ezorm.rdb.codec;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Setter;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.rdb.utils.FeatureUtils;
import reactor.core.publisher.Flux;

import java.io.InputStream;
import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.sql.Blob;
import java.sql.Clob;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

@Slf4j
public class JsonValueCodec implements ValueCodec<Object, Object> {

    public static final ObjectMapper defaultMapper = new ObjectMapper();

    static {
        defaultMapper.disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES);
        defaultMapper.setTimeZone(TimeZone.getDefault());
    }

    private JavaType jacksonType;

    @Setter
    private ObjectMapper mapper = defaultMapper;

    public static JsonValueCodec of(Class targetType) {
        return new JsonValueCodec(defaultMapper.getTypeFactory().constructType(targetType));
    }

    public static JsonValueCodec ofCollection(Class<? extends Collection> targetType
            , Class elementType) {
        return new JsonValueCodec(defaultMapper.getTypeFactory().constructCollectionType(targetType, elementType));
    }

    public static JsonValueCodec ofMap(Class<? extends Map> targetType, Class keyType, Class valueType) {
        return new JsonValueCodec(defaultMapper.getTypeFactory()
                .constructMapType(targetType, keyType, valueType));
    }

    public static JsonValueCodec ofField(Field field) {
        Class type = field.getType();
        Type genericType = field.getGenericType();
        JavaType jacksonType = null;
        if (Map.class.isAssignableFrom(type)) {
            if (genericType instanceof ParameterizedType) {
                Type[] types = ((ParameterizedType) genericType).getActualTypeArguments();
                jacksonType = defaultMapper
                        .getTypeFactory()
                        .constructMapType(type, (Class) types[0], (Class) types[1]);
            }

        } else if (Collection.class.isAssignableFrom(type)) {
            if (genericType instanceof ParameterizedType) {
                Type[] types = ((ParameterizedType) genericType).getActualTypeArguments();
                jacksonType = defaultMapper
                        .getTypeFactory()
                        .constructCollectionType(type, (Class) types[0]);
            }
        } else if (type.isArray()) {
            jacksonType = defaultMapper
                    .getTypeFactory()
                    .constructArrayType(type.getComponentType());
        }
        if (jacksonType == null) {
            jacksonType = defaultMapper.getTypeFactory().constructType(type);
        }

        return new JsonValueCodec(jacksonType);
    }


    public JsonValueCodec(JavaType type) {
        this.jacksonType = type;
    }

    @Override
    @SneakyThrows
    public Object encode(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof String) {
            return value;
        }
        return mapper.writeValueAsString(value);
    }

    @SneakyThrows
    protected Object doRead(String str) {
        return mapper.readValue(str, jacksonType);

    }

    @Override
    @SneakyThrows
    public Object decode(Object data) {
        if (data instanceof Clob) {
            return mapper.readValue(((Clob) data).getCharacterStream(), jacksonType);
        }
        if (FeatureUtils.r2dbcIsAlive()) {
            if (data instanceof io.r2dbc.spi.Clob) {
                return Flux.from(((io.r2dbc.spi.Clob) data).stream())
                        .collect(Collectors.joining())
                        .map(this::doRead)
                        .toFuture()
                        .get(10, TimeUnit.SECONDS);
            }
        }

        if (data instanceof Blob) {
            return mapper.readValue(((Blob) data).getBinaryStream(), jacksonType);
        }
        if (data instanceof InputStream) {
            return mapper.readValue((InputStream) data, jacksonType);
        }
        if (data instanceof byte[]) {
            return mapper.readValue((byte[]) data, jacksonType);
        }
        if (data instanceof String) {
            return mapper.readValue(((String) data), jacksonType);
        }
        log.warn("unsupported json format:{}", data);
        return data;
    }
}
