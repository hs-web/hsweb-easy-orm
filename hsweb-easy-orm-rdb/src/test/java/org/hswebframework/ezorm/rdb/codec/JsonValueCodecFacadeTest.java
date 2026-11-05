package org.hswebframework.ezorm.rdb.codec;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.r2dbc.spi.Blob;
import io.r2dbc.spi.Clob;
import org.jetlinks.json.JsonType;
import org.jetlinks.json.JsonException;
import org.reactivestreams.Publisher;
import org.junit.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.io.ByteArrayInputStream;
import java.io.StringReader;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import javax.sql.rowset.serial.SerialBlob;
import javax.sql.rowset.serial.SerialClob;

import static org.junit.Assert.*;

public class JsonValueCodecFacadeTest {
    @Test
    public void genericBindingAndNativeConfigurationRemainIndependent() {
        JsonValueCodec codec = JsonValueCodec.of(new JsonType<Map<String, List<Item>>>() {});
        Map<?, ?> decoded = (Map<?, ?>) codec.decode("{\"items\":[{\"name\":\"test\"}]}");
        assertEquals("test", ((Item) ((List<?>) decoded.get("items")).get(0)).name);
        assertEquals("{}", codec.encode(Collections.singletonMap("missing", null)));

        ObjectMapper nativeMapper = new ObjectMapper();
        JsonValueCodec legacy = JsonValueCodec.of(Map.class);
        legacy.setMapper(nativeMapper);
        // setter 保留对象身份：调用之后的原生配置仍须生效，不能在 setter 内偷偷复制。
        nativeMapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        assertEquals("{}", legacy.encode(Collections.singletonMap("missing", null)));
        assertEquals("{\"a\":1}", codec.encode("{\"a\":1}"));

        JsonValueCodec array = JsonValueCodec.of(new JsonType<List<Item>[]>() {});
        List<?>[] values = (List<?>[]) array.decode("[[{\"name\":\"array\"}]]");
        assertEquals("array", ((Item) values[0].get(0)).name);
    }

    @Test
    public void borrowedIoAndBufferIndicesArePreserved() {
        JsonValueCodec codec = JsonValueCodec.of(JsonType.of(Item.class));
        AtomicBoolean closed = new AtomicBoolean();
        var input = new ByteArrayInputStream("{\"name\":\"test\"}".getBytes(StandardCharsets.UTF_8)) {
            @Override public void close() { closed.set(true); }
        };
        assertEquals("test", ((Item) codec.decode(input)).name);
        assertFalse(closed.get());
        assertEquals("test", ((Item) codec.decode(new StringReader("{\"name\":\"test\"}"))).name);
        ByteBuffer buffer = ByteBuffer.wrap(" {\"name\":\"test\"}".getBytes(StandardCharsets.UTF_8));
        buffer.position(1);
        assertEquals("test", ((Item) codec.decode(buffer)).name);
        assertEquals(1, buffer.position());
    }

    @Test
    public void jdbcLobStreamsAreClosedOnceAndNativeOwnershipIsPreserved() throws Exception {
        AtomicInteger closes = new AtomicInteger();
        byte[] bytes = "{\"name\":\"test\"}".getBytes(StandardCharsets.UTF_8);
        var blob = new SerialBlob(bytes) {
            @Override public java.io.InputStream getBinaryStream() {
                return new ByteArrayInputStream(bytes) {
                    @Override public void close() { closes.incrementAndGet(); }
                };
            }
        };
        var clob = new SerialClob("{\"name\":\"test\"}".toCharArray()) {
            @Override public java.io.Reader getCharacterStream() {
                return new StringReader("{\"name\":\"test\"}") {
                    @Override public void close() { closes.incrementAndGet(); }
                };
            }
        };
        JsonValueCodec codec = JsonValueCodec.of(JsonType.of(Item.class));
        assertEquals("test", ((Item) codec.decode(blob)).name);
        assertEquals("test", ((Item) codec.decode(clob)).name);
        assertEquals(2, closes.get());

        JsonValueCodec legacy = JsonValueCodec.of(Item.class);
        legacy.setMapper(new ObjectMapper().disable(com.fasterxml.jackson.core.JsonParser.Feature.AUTO_CLOSE_SOURCE));
        assertEquals("test", ((Item) legacy.decode(blob)).name);
        assertEquals(2, closes.get());
    }

    @Test
    public void reactiveLobReadsEveryFragmentWithoutEarlySubscription() throws Exception {
        JsonValueCodec codec = JsonValueCodec.ofField(Holder.class.getDeclaredField("items"), JsonValueCodec.defaultJsonMapper);
        byte[] bytes = "[{\"name\":\"中文😀\"}]".getBytes(StandardCharsets.UTF_8);
        AtomicBoolean subscribed = new AtomicBoolean();
        Flux<ByteBuffer> fragments = Flux.range(0, bytes.length)
            .map(index -> ByteBuffer.wrap(Arrays.copyOfRange(bytes, index, index + 1)))
            .doOnSubscribe(ignored -> subscribed.set(true));
        Object value = codec.decode(Blob.from(fragments));
        assertFalse(subscribed.get());
        StepVerifier.create((Mono<?>) value)
            .assertNext(result -> assertEquals("中文😀", ((Item) ((List<?>) result).get(0)).name))
            .verifyComplete();
        Object clob = codec.decode(Clob.from(Flux.just("[{\"name\":\"\uD83D", "\uDE00\"}]")));
        StepVerifier.create((Mono<?>) clob)
            .assertNext(result -> assertEquals("😀", ((Item) ((List<?>) result).get(0)).name))
            .verifyComplete();
    }

    @Test
    public void reactiveLobCancellationAndErrorsReachSource() throws Exception {
        JsonValueCodec codec = JsonValueCodec.ofField(Holder.class.getDeclaredField("items"), JsonValueCodec.defaultJsonMapper);
        AtomicBoolean cancelled = new AtomicBoolean();
        Object value = codec.decode(Blob.from(Flux.<ByteBuffer>never().doOnCancel(() -> cancelled.set(true))));
        StepVerifier.create((Mono<?>) value).thenCancel().verify();
        assertTrue(cancelled.get());
        RuntimeException failure = new IllegalStateException("source failed");
        Object error = codec.decode(Blob.from(Flux.error(failure)));
        StepVerifier.create((Mono<?>) error).expectErrorMatches(e -> e == failure).verify();
        cancelled.set(false);
        Object text = codec.decode(Clob.from(Flux.<CharSequence>never().doOnCancel(() -> cancelled.set(true))));
        StepVerifier.create((Mono<?>) text).thenCancel().verify();
        assertTrue(cancelled.get());
        StepVerifier.create((Mono<?>) codec.decode(Clob.from(Flux.error(failure))))
            .expectErrorMatches(e -> e == failure).verify();
    }

    @Test
    public void publisherFieldsEmitOneCompleteColumnAndSkipJsonNull() throws Exception {
        for (String field : List.of("items", "flux", "publisher")) {
            JsonValueCodec codec = JsonValueCodec.ofField(Holder.class.getDeclaredField(field), JsonValueCodec.defaultJsonMapper);
            for (boolean binary : List.of(true, false)) {
                Object emptyArray = codec.decode(lob("[]", binary));
                StepVerifier.create((Publisher<?>) emptyArray).expectNextMatches(List.of()::equals).verifyComplete();
                Object value = codec.decode(lob("[{\"name\":\"one\"},{\"name\":\"two\"}]", binary));
                StepVerifier.create((Publisher<?>) value)
                    .assertNext(result -> {
                        List<?> items = (List<?>) result;
                        assertEquals(2, items.size());
                        assertEquals("two", ((Item) items.get(1)).name);
                    }).verifyComplete();
                StepVerifier.create((Publisher<?>) codec.decode(lob("null", binary))).verifyComplete();
            }
        }
    }

    @Test
    public void reactiveLobRejectsIncompleteAndTrailingDocuments() throws Exception {
        JsonValueCodec codec = JsonValueCodec.ofField(Holder.class.getDeclaredField("items"), JsonValueCodec.defaultJsonMapper);
        for (boolean binary : List.of(true, false)) {
            for (String invalid : List.of("[{", "[] []", " ")) {
                StepVerifier.create((Publisher<?>) codec.decode(lob(invalid, binary)))
                    .expectError(JsonException.class).verify();
            }
        }
        StepVerifier.create((Publisher<?>) codec.decode(Blob.from(Flux.empty())))
            .expectError(JsonException.class).verify();
        // 空字符串沿用 doRead(String) 的历史空值规则。
        StepVerifier.create((Publisher<?>) codec.decode(Clob.from(Flux.empty()))).verifyComplete();
    }

    private static Object lob(String text, boolean binary) {
        return binary ? Blob.from(Mono.just(ByteBuffer.wrap(text.getBytes(StandardCharsets.UTF_8))))
            : Clob.from(Mono.just(text));
    }

    @Test
    public void interruptedSynchronousLobReadCancelsSource() throws Exception {
        CountDownLatch subscribed = new CountDownLatch(1);
        AtomicBoolean cancelled = new AtomicBoolean();
        JsonValueCodec codec = JsonValueCodec.of(JsonType.of(Item.class));
        Blob blob = Blob.from(Flux.<ByteBuffer>never()
            .doOnSubscribe(ignored -> subscribed.countDown())
            .doOnCancel(() -> cancelled.set(true)));
        AtomicReference<Object> result = new AtomicReference<>(new Object());
        Thread reader = new Thread(() -> result.set(codec.decode(blob)));
        reader.start();
        try {
            assertTrue(subscribed.await(5, TimeUnit.SECONDS));
        } finally {
            reader.interrupt();
            reader.join(5000);
        }
        assertFalse(reader.isAlive());
        assertNull(result.get());
        assertTrue(cancelled.get());
    }

    @Test
    public void protectedLegacyReadHooksRetainDispatch() {
        AtomicInteger streamCalls = new AtomicInteger();
        JsonValueCodec legacy = new JsonValueCodec(Map.class, JsonValueCodec.defaultMapper.constructType(Map.class)) {
            @Override protected Object doRead(String text) { return Map.of("hook", "text"); }
            @Override protected Object doRead(java.io.InputStream stream) {
                streamCalls.incrementAndGet();
                return Map.of("hook", "buffer");
            }
        };
        assertEquals(Map.of("hook", "text"), legacy.decode("{}"));
        assertEquals(Map.of(), legacy.decode(new ByteArrayInputStream("{}".getBytes(StandardCharsets.UTF_8))));
        assertEquals(Map.of("hook", "buffer"), legacy.decode(ByteBuffer.wrap("{}".getBytes(StandardCharsets.UTF_8))));
        assertEquals(1, streamCalls.get());
    }

    public static class Holder {
        public Mono<List<Item>> items;
        public Flux<List<Item>> flux;
        public Publisher<List<Item>> publisher;
    }

    public static class Item {
        public String name;
    }
}
