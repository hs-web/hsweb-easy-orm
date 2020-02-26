package org.hswebframework.ezorm.rdb.codec;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.util.ByteBufferBackedInputStream;
import lombok.Setter;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.rdb.utils.FeatureUtils;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.io.InputStream;
import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
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

    private Class targetType;

    @Setter
    private ObjectMapper mapper = defaultMapper;

    public static JsonValueCodec of(Class targetType) {
        return new JsonValueCodec(targetType, defaultMapper.getTypeFactory().constructType(targetType));
    }

    public static JsonValueCodec ofCollection(Class<? extends Collection> targetType
            , Class elementType) {
        return new JsonValueCodec(targetType, defaultMapper.getTypeFactory().constructCollectionType(targetType, elementType));
    }

    public static JsonValueCodec ofMap(Class<? extends Map> targetType, Class keyType, Class valueType) {
        return new JsonValueCodec(targetType, defaultMapper.getTypeFactory()
                .constructMapType(targetType, keyType, valueType));
    }

    public static JsonValueCodec ofField(Field field) {
        Class type = field.getType();
        Class targetType = type;
        Type genericType = field.getGenericType();
        JavaType jacksonType = null;

        if (type == Mono.class || type == Flux.class) {
            targetType = (Class) ((ParameterizedType) genericType).getActualTypeArguments()[0];
        }
        if (Map.class.isAssignableFrom(targetType)) {
            if (genericType instanceof ParameterizedType) {
                Type[] types = ((ParameterizedType) genericType).getActualTypeArguments();
                jacksonType = defaultMapper
                        .getTypeFactory()
                        .constructMapType(targetType, (Class) types[0], (Class) types[1]);
            }

        } else if (Collection.class.isAssignableFrom(targetType)) {
            if (genericType instanceof ParameterizedType) {
                Type[] types = ((ParameterizedType) genericType).getActualTypeArguments();
                jacksonType = defaultMapper
                        .getTypeFactory()
                        .constructCollectionType(targetType, (Class) types[0]);
            }
        } else if (targetType.isArray()) {
            jacksonType = defaultMapper
                    .getTypeFactory()
                    .constructArrayType(targetType.getComponentType());
        }
        if (jacksonType == null) {
            jacksonType = defaultMapper.getTypeFactory().constructType(targetType);
        }

        return new JsonValueCodec(type, jacksonType);
    }


    public JsonValueCodec(Class targetType, JavaType type) {
        this.jacksonType = type;
        this.targetType = targetType;
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

    @SneakyThrows
    protected Object doRead(InputStream stream) {
        return mapper.readValue(stream, jacksonType);
    }

    @Override
    @SneakyThrows
    public Object decode(Object data) {

        Object target = data;

        if (data instanceof Clob) {
            target = mapper.readValue(((Clob) data).getCharacterStream(), jacksonType);
        } else if (data instanceof Blob) {
            target = mapper.readValue(((Blob) data).getBinaryStream(), jacksonType);
        } else if (data instanceof InputStream) {
            target = mapper.readValue((InputStream) data, jacksonType);
        } else if (data instanceof byte[]) {
            target = mapper.readValue((byte[]) data, jacksonType);
        } else if (data instanceof String) {
            target = mapper.readValue(((String) data), jacksonType);
        }else if(data instanceof ByteBuffer){
            return doRead(new ByteBufferBackedInputStream(((ByteBuffer) data)));
        }
        else if (FeatureUtils.r2dbcIsAlive()) {
            Mono mono = null;
            if (data instanceof io.r2dbc.spi.Clob) {
                mono = Flux.from(((io.r2dbc.spi.Clob) data).stream())
                        .collect(Collectors.joining())
                        .map(this::doRead);

            } else if (data instanceof io.r2dbc.spi.Blob) {
                mono = Mono.from(((io.r2dbc.spi.Blob) data).stream())
                        .map(ByteBufferBackedInputStream::new)
                        .map(this::doRead);
            }
            if (mono != null) {
                if (targetType == Mono.class || targetType == Publisher.class) {
                    return mono;
                }
                if (targetType == Flux.class) {
                    return mono.flux();
                }
                // TODO: 2019-09-25 更好的方式？
                target = mono.toFuture().get(10, TimeUnit.SECONDS);
            }
        }

        if (targetType.isInstance(target)) {
            return target;
        }
        if (targetType == Mono.class || targetType == Publisher.class) {
            return target == null ? Mono.empty() : Mono.just(target);
        }
        if (targetType == Flux.class) {
            return target == null ? Flux.empty() : Flux.just(target);
        }
        log.warn("unsupported json format:{}", data);
        return target;
    }
}
