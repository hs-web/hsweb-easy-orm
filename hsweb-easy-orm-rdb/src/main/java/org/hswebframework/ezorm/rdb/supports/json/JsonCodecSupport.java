package org.hswebframework.ezorm.rdb.supports.json;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.codec.JsonValueCodec;
import org.hswebframework.ezorm.rdb.utils.FeatureUtils;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.Reader;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.sql.Blob;
import java.sql.Clob;
import java.util.Iterator;
import java.util.List;
import java.util.ServiceConfigurationError;
import java.util.ServiceLoader;
import java.util.concurrent.CopyOnWriteArrayList;

public final class JsonCodecSupport {

    private static final List<JsonStringReader> stringReaders = new CopyOnWriteArrayList<>();

    static {
        loadStringReaders();
    }

    private JsonCodecSupport() {
    }

    public static void registerStringReader(JsonStringReader reader) {
        if (reader != null) {
            stringReaders.add(reader);
        }
    }

    private static void loadStringReaders() {
        ServiceLoader<JsonStringReader> loader = ServiceLoader.load(JsonStringReader.class);
        Iterator<JsonStringReader> iterator = loader.iterator();
        while (true) {
            boolean hasNext;
            try {
                hasNext = iterator.hasNext();
            } catch (ServiceConfigurationError ignore) {
                return;
            }
            if (!hasNext) {
                return;
            }
            try {
                registerStringReader(iterator.next());
            } catch (ServiceConfigurationError ignore) {
                // Optional database drivers may be absent at runtime. Skip their readers.
            }
        }
    }

    @SneakyThrows
    public static String toJson(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof CharSequence) {
            return value.toString();
        }
        return JsonValueCodec.defaultMapper.writeValueAsString(value);
    }

    public static String toJsonSilently(Object value) {
        try {
            return toJson(value);
        } catch (Throwable e) {
            throw new IllegalArgumentException("Unsupported json value: " + value, e);
        }
    }

    public static boolean canReadAsString(Object data) {
        if (data == null) {
            return false;
        }
        if (data instanceof CharSequence ||
            data instanceof Clob ||
            data instanceof Blob ||
            data instanceof byte[] ||
            data instanceof ByteBuffer ||
            data instanceof InputStream ||
            data instanceof Reader) {
            return true;
        }
        if (findStringReader(data) != null) {
            return true;
        }
        return FeatureUtils.r2dbcIsAlive() && (data instanceof io.r2dbc.spi.Clob || data instanceof io.r2dbc.spi.Blob);
    }

    @SneakyThrows
    public static String readAsString(Object data) {
        if (data == null) {
            return null;
        }
        if (data instanceof CharSequence) {
            return data.toString();
        }
        JsonStringReader reader = findStringReader(data);
        if (reader != null) {
            return reader.read(data);
        }
        if (data instanceof Clob clob) {
            return read(clob.getCharacterStream());
        }
        if (data instanceof Blob blob) {
            return read(blob.getBinaryStream());
        }
        if (data instanceof byte[] bytes) {
            return new String(bytes, StandardCharsets.UTF_8);
        }
        if (data instanceof ByteBuffer buffer) {
            ByteBuffer duplicate = buffer.asReadOnlyBuffer();
            byte[] bytes = new byte[duplicate.remaining()];
            duplicate.get(bytes);
            return new String(bytes, StandardCharsets.UTF_8);
        }
        if (data instanceof InputStream stream) {
            return read(stream);
        }
        if (data instanceof Reader dataReader) {
            return read(dataReader);
        }
        if (FeatureUtils.r2dbcIsAlive()) {
            String text = tryReadR2dbcLob(data);
            if (text != null) {
                return text;
            }
        }
        return String.valueOf(data);
    }

    private static JsonStringReader findStringReader(Object data) {
        for (JsonStringReader reader : stringReaders) {
            if (reader.supports(data)) {
                return reader;
            }
        }
        return null;
    }

    @SneakyThrows
    private static String tryReadR2dbcLob(Object data) {
        if (data instanceof io.r2dbc.spi.Clob clob) {
            return reactor.core.publisher.Flux.from(clob.stream())
                                           .collectList()
                                           .map(list -> String.join("", list))
                                           .toFuture()
                                           .get();
        }
        if (data instanceof io.r2dbc.spi.Blob blob) {
            return reactor.core.publisher.Flux.from(blob.stream())
                                           .collectList()
                                           .map(buffers -> {
                                               int len = buffers.stream().mapToInt(ByteBuffer::remaining).sum();
                                               byte[] bytes = new byte[len];
                                               int offset = 0;
                                               for (ByteBuffer buffer : buffers) {
                                                   ByteBuffer duplicate = buffer.asReadOnlyBuffer();
                                                   int remaining = duplicate.remaining();
                                                   duplicate.get(bytes, offset, remaining);
                                                   offset += remaining;
                                               }
                                               return new String(bytes, StandardCharsets.UTF_8);
                                           })
                                           .toFuture()
                                           .get();
        }
        return null;
    }

    @SneakyThrows
    private static String read(InputStream stream) {
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        byte[] buffer = new byte[4096];
        int len;
        while ((len = stream.read(buffer)) != -1) {
            output.write(buffer, 0, len);
        }
        return output.toString(StandardCharsets.UTF_8.name());
    }

    @SneakyThrows
    private static String read(Reader reader) {
        StringBuilder builder = new StringBuilder();
        char[] buffer = new char[4096];
        int len;
        while ((len = reader.read(buffer)) != -1) {
            builder.append(buffer, 0, len);
        }
        return builder.toString();
    }
}
