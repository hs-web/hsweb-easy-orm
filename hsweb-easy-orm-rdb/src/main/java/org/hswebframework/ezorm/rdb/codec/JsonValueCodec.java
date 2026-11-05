package org.hswebframework.ezorm.rdb.codec;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.util.ByteBufferBackedInputStream;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.core.meta.ColumnMetadata;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.supports.json.JsonCodecSupport;
import org.hswebframework.ezorm.rdb.utils.FeatureUtils;
import org.reactivestreams.Publisher;
import org.jetlinks.json.JsonMapper;
import org.jetlinks.json.JsonReadOptions;
import org.jetlinks.json.JsonType;
import org.jetlinks.json.jackson2.Jackson2Provider;
import org.jetlinks.json.reactor.JsonStreamMapper;
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
import java.util.Objects;
import java.util.TimeZone;
import java.util.concurrent.TimeUnit;

/**
 * JSON 列编解码。新入口传入 JsonType 和稳定的 JsonMapper；旧工厂继续跟随可变 defaultMapper。
 * 字符串写入保持透传。decode 保留历史同步错误返回 null 的契约，响应式 LOB 在订阅时解析。
 * 中立入口借用调用方的流，仅关闭 JDBC LOB 创建的流；旧入口仍遵循原生 mapper 的关闭设置。
 * 映射器配置应在并发使用前完成。响应式字段延迟读取 LOB，同步字段仍等待完整列值。
 *
 * @see #of(JsonType, JsonMapper)
 */
@Slf4j
public class JsonValueCodec implements ValueCodec<Object, Object> {

    /** @deprecated 原生可变配置仅为旧入口保留；新入口使用 {@link #defaultJsonMapper}。 */
    @Deprecated
    public static final ObjectMapper defaultMapper;

    /** 独立的持久化配置快照，不使用 HTTP 或 Spring 全局映射器。 */
    public static final JsonMapper defaultJsonMapper;

    static {
        defaultMapper = new ObjectMapper()
            .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
            .setDefaultPropertyInclusion(JsonInclude.Include.NON_NULL)
            .setTimeZone(TimeZone.getDefault());
        defaultJsonMapper = Jackson2Provider.adapt(defaultMapper);
    }

    private final JavaType jacksonType;

    private final Class<?> targetType;

    private final JsonType<?> jsonType;

    private JsonMapper jsonMapper;

    private ObjectMapper mapper = defaultMapper;

    /**
     * @deprecated 保留原对象及其后续配置变更；新代码通过 {@link #of(JsonType, JsonMapper)} 指定配置。
     */
    @Deprecated
    public void setMapper(ObjectMapper mapper) {
        this.mapper = Objects.requireNonNull(mapper, "mapper");
        this.jsonMapper = null;
    }

    /** 使用独立的默认持久化配置，例如 {@code of(new JsonType<List<Item>>() {})}。 */
    public static JsonValueCodec of(JsonType<?> type) {
        return of(type, defaultJsonMapper);
    }

    /** 使用已完成配置的门面；泛型由 Provider 处理，不经过字符串或 Map 中转。 */
    public static JsonValueCodec of(JsonType<?> type, JsonMapper mapper) {
        return new JsonValueCodec(null, type, mapper);
    }

    private JsonValueCodec(Class<?> targetType, JsonType<?> type, JsonMapper mapper) {
        this.jsonType = Objects.requireNonNull(type, "type");
        this.jsonMapper = Objects.requireNonNull(mapper, "mapper");
        // 仅供弃用 setMapper 切回旧路径时使用；不参与新入口的绑定。
        this.jacksonType = defaultMapper.constructType(type.type());
        this.targetType = targetType == null ? jacksonType.getRawClass() : targetType;
    }

    /**
     * 按字段完整泛型创建 codec；Mono/Flux/Publisher 只展开一层，仍绑定一个完整 JSON 列值。
     * 例如 Mono<List<Item>> 返回 Mono 中的 List，不把数组元素拆成数据库记录。
     */
    public static JsonValueCodec ofField(Field field, JsonMapper mapper) {
        return new JsonValueCodec(field.getType(), JsonType.of(fieldValueType(field)), mapper);
    }

    /** 保留旧原生配置路径；使用稳定配置时改用 {@code of(JsonType.of(targetType))}。 */
    public static JsonValueCodec of(Class<?> targetType) {
        return new JsonValueCodec(targetType, defaultMapper.getTypeFactory().constructType(targetType));
    }

    /** 按元素类型创建旧集合 codec；嵌套泛型使用 {@link #of(JsonType)}。 */
    public static JsonValueCodec ofCollection(Class<? extends Collection> targetType
        , Class<?> elementType) {
        return new JsonValueCodec(targetType, defaultMapper
            .getTypeFactory()
            .constructCollectionType(targetType, elementType));
    }

    /** 按键值类型创建旧 Map codec，继续跟随 {@link #defaultMapper}。 */
    public static JsonValueCodec ofMap(Class<? extends Map> targetType, Class<?> keyType, Class<?> valueType) {
        return new JsonValueCodec(targetType, defaultMapper.getTypeFactory()
                                                           .constructMapType(targetType, keyType, valueType));
    }

    /** 按字段完整泛型绑定，保留 defaultMapper 的配置；稳定配置使用 {@link #ofField(Field, JsonMapper)}。 */
    public static JsonValueCodec ofField(Field field) {
        return new JsonValueCodec(field.getType(), defaultMapper.constructType(fieldValueType(field)));
    }

    private static Type fieldValueType(Field field) {
        Type type = field.getGenericType();
        // 一个 Publisher 元素对应一个完整列值，嵌套 Map/List 泛型交给 Provider 解析。
        if (Publisher.class.isAssignableFrom(field.getType())) {
            return type instanceof ParameterizedType p ? p.getActualTypeArguments()[0] : Object.class;
        }
        return type;
    }

    /** @deprecated 使用 {@link #of(JsonType, JsonMapper)}，避免依赖 Jackson JavaType。 */
    @Deprecated
    public JsonValueCodec(Class<?> targetType, JavaType type) {
        this.jacksonType = type;
        this.targetType = targetType;
        this.jsonType = null;
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
        return jsonMapper == null ? mapper.writeValueAsString(value) : jsonMapper.write(value);
    }

    @SneakyThrows
    protected Object doRead(String str) {
        if (str.isEmpty()) {
            return null;
        }
        return jsonMapper == null ? mapper.readValue(str, jacksonType) : jsonMapper.read(str, jsonType);
    }

    @SneakyThrows
    protected Object doRead(InputStream stream) {
        return jsonMapper == null ? mapper.readValue(stream, jacksonType) : jsonMapper.read(stream, jsonType);
    }

    @SneakyThrows
    private Object doRead(Reader reader) {
        return jsonMapper == null ? mapper.readValue(reader, jacksonType) : jsonMapper.read(reader, jsonType);
    }

    @SneakyThrows
    private Object finishLob(Mono<?> value) {
        if (targetType == Mono.class || targetType == Publisher.class) {
            return value;
        }
        if (targetType == Flux.class) {
            return value.flux();
        }
        // 兼容已有同步 codec 契约；响应式字段在上面的分支直接返回，不阻塞订阅线程。
        var future = value.toFuture();
        try {
            return future.get(10, TimeUnit.SECONDS);
        } finally {
            // 等待超时或被中断时也取消上游；已完成的 future 不受影响。
            future.cancel(true);
        }
    }

    @Override
    @SneakyThrows
    public Object decode(Object data) {
        try {
            Object target = data;

            if (jsonMapper != null && FeatureUtils.r2dbcIsAlive()) {
                if (data instanceof io.r2dbc.spi.Blob blob) {
                    var options = new JsonReadOptions(JsonReadOptions.Mode.SINGLE_VALUE, null,
                        JsonReadOptions.NullPolicy.SKIP, jsonMapper.options().maxValueBytes());
                    Mono<?> decoded = JsonStreamMapper.of(jsonMapper)
                        .readBuffers(blob.stream(), jsonType, options).singleOrEmpty();
                    return finishLob(decoded);
                }
                if (data instanceof io.r2dbc.spi.Clob clob) {
                    // CLOB 是字符流，完整列值一次绑定；拼接字符可保留跨分片的 UTF-16 代理对。
                    Mono<?> decoded = Flux.from(clob.stream())
                        .collect(StringBuilder::new, StringBuilder::append)
                        .flatMap(text -> Mono.justOrEmpty(doRead(text.toString())));
                    return finishLob(decoded);
                }
            }
            if (data instanceof Clob _clob) {
                if (jsonMapper == null) {
                    target = mapper.readValue(_clob.getCharacterStream(), jacksonType);
                } else {
                    try (Reader reader = _clob.getCharacterStream()) {
                        target = doRead(reader);
                    }
                }
            } else if (data instanceof Blob _blob) {
                if (jsonMapper == null) {
                    target = mapper.readValue(_blob.getBinaryStream(), jacksonType);
                } else {
                    try (InputStream stream = _blob.getBinaryStream()) {
                        target = doRead(stream);
                    }
                }
            } else if (data instanceof InputStream) {
                // 旧路径原本不调用受保护方法，避免改变已有子类的分派行为。
                target = jsonMapper == null ? mapper.readValue((InputStream) data, jacksonType)
                    : doRead((InputStream) data);
            } else if (data instanceof byte[] bytes) {
                target = jsonMapper == null ? mapper.readValue(bytes, jacksonType) : jsonMapper.read(bytes, jsonType);
            } else if (data instanceof CharSequence) {
                target = doRead(String.valueOf(data));
            } else if (data instanceof Reader reader) {
                target = doRead(reader);
            } else if (data instanceof ByteBuffer) {
                if (jsonMapper == null) {
                    return doRead(new ByteBufferBackedInputStream(((ByteBuffer) data)));
                }
                target = doRead(new ByteBufferBackedInputStream(((ByteBuffer) data).asReadOnlyBuffer()));
            } else if (JsonCodecSupport.canReadAsString(data)) {
                target = doRead(JsonCodecSupport.readAsString(data));
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
