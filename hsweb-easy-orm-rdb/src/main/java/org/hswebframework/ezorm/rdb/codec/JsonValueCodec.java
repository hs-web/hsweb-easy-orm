package org.hswebframework.ezorm.rdb.codec;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.type.TypeFactory;
import com.fasterxml.jackson.databind.util.ByteBufferBackedInputStream;
import lombok.Setter;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.core.meta.ColumnMetadata;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.supports.json.JsonCodecSupport;
import org.hswebframework.ezorm.rdb.utils.FeatureUtils;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.io.InputStream;
import java.io.Reader;
import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.nio.ByteBuffer;
import java.sql.Blob;
import java.sql.Clob;
import java.util.Collection;
import java.util.Map;
import java.util.TimeZone;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

@Slf4j
public class JsonValueCodec implements ValueCodec<Object, Object> {

    public static final ObjectMapper defaultMapper;

    static {
        defaultMapper = new ObjectMapper()
            .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
            .setDefaultPropertyInclusion(JsonInclude.Include.NON_NULL)
            .setTimeZone(TimeZone.getDefault());
    }

    private final JavaType jacksonType;

    private final Class<?> targetType;

    @Setter
    private ObjectMapper mapper = defaultMapper;

    public static JsonValueCodec of(Class<?> targetType) {
        return new JsonValueCodec(targetType, defaultMapper.getTypeFactory().constructType(targetType));
    }

    public static JsonValueCodec ofCollection(Class<? extends Collection> targetType
        , Class<?> elementType) {
        return new JsonValueCodec(targetType, defaultMapper
            .getTypeFactory()
            .constructCollectionType(targetType, elementType));
    }

    public static JsonValueCodec ofMap(Class<? extends Map> targetType, Class<?> keyType, Class<?> valueType) {
        return new JsonValueCodec(targetType, defaultMapper.getTypeFactory()
                                                           .constructMapType(targetType, keyType, valueType));
    }

    @SuppressWarnings("all")
    public static JsonValueCodec ofField(Field field) {
        Class type = field.getType();
        Class targetType = type;
        Type genericType = field.getGenericType();
        JavaType jacksonType = null;

        if (type == Mono.class || type == Flux.class) {
            targetType = (Class<?>) ((ParameterizedType) genericType).getActualTypeArguments()[0];
        }
        TypeFactory factory = defaultMapper.getTypeFactory();
        if (Map.class.isAssignableFrom(targetType)) {
            if (genericType instanceof ParameterizedType) {
                Type[] types = ((ParameterizedType) genericType).getActualTypeArguments();
                jacksonType = factory.constructMapType(targetType, factory.constructType(types[0]), factory.constructType(types[1]));
            }

        } else if (Collection.class.isAssignableFrom(targetType)) {
            if (genericType instanceof ParameterizedType) {
                Type[] types = ((ParameterizedType) genericType).getActualTypeArguments();
                jacksonType = factory.constructCollectionType(targetType, factory.constructType(types[0]));
            }
        } else if (targetType.isArray()) {
            jacksonType = factory.constructArrayType(targetType.getComponentType());
        }
        if (jacksonType == null) {
            jacksonType = factory.constructType(targetType);
        }

        return new JsonValueCodec(type, jacksonType);
    }


    public JsonValueCodec(Class<?> targetType, JavaType type) {
        this.jacksonType = type;
        this.targetType = targetType;
    }

    @Override
    public Object encodeNull(ColumnMetadata column) {
        if (column instanceof RDBColumnMetadata col) {
            // clob 类型
            if (ClobValueCodec.isClobType(col.getType())) {
                return NullValue.of(LongCharSequence.class, col.getType());
            }
            return NullValue.of(col.getType());
        }
        return ValueCodec.super.encodeNull(column);
    }

    @Override
    public Object encode(Object value, ColumnMetadata column) {
        Object data = encode(value);
        if (data instanceof CharSequence cs && column instanceof RDBColumnMetadata col) {
            // clob 类型
            if (ClobValueCodec.isClobType(col.getType())) {
                return new LongCharSequence(cs);
            }
        }
        return data;
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
        if (str.isEmpty()) {
            return null;
        }
        return mapper.readValue(str, jacksonType);
    }

    @SneakyThrows
    protected Object doRead(InputStream stream) {
        return mapper.readValue(stream, jacksonType);
    }

    @Override
    @SneakyThrows
    public Object decode(Object data) {
        try {
            Object target = data;

            if (data instanceof Clob _clob) {
                target = mapper.readValue(_clob.getCharacterStream(), jacksonType);
            } else if (data instanceof Blob _blob) {
                target = mapper.readValue(_blob.getBinaryStream(), jacksonType);
            } else if (data instanceof InputStream) {
                target = mapper.readValue((InputStream) data, jacksonType);
            } else if (data instanceof byte[] bytes) {
                target = mapper.readValue(bytes, jacksonType);
            } else if (data instanceof CharSequence) {
                target = doRead(String.valueOf(data));
            } else if (data instanceof Reader reader) {
                target = mapper.readValue(reader, jacksonType);
            } else if (data instanceof ByteBuffer) {
                return doRead(new ByteBufferBackedInputStream(((ByteBuffer) data)));
            } else if (JsonCodecSupport.canReadAsString(data)) {
                target = doRead(JsonCodecSupport.readAsString(data));
            } else if (FeatureUtils.r2dbcIsAlive()) {
                Mono<?> mono = null;
                if (data instanceof io.r2dbc.spi.Clob _clob) {
                    mono = Flux.from(_clob.stream())
                               .collect(Collectors.joining())
                               .map(this::doRead);

                } else if (data instanceof io.r2dbc.spi.Blob _blob) {
                    mono = Mono.from(_blob.stream())
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
            if (target == null) {
                return null;
            }
            log.warn("unsupported json format:{}", data);
            return target;
        } catch (Throwable e) {
            log.error("decode json error {}", data, e);
            return null;
        }
    }
}
