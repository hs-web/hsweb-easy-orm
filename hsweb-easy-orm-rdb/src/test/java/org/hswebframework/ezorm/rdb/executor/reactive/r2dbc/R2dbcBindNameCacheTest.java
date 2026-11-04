package org.hswebframework.ezorm.rdb.executor.reactive.r2dbc;

import io.r2dbc.spi.Connection;
import io.r2dbc.spi.Statement;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.R2dbcParameterBinder;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.junit.Test;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;

import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.*;

import static org.junit.Assert.*;

public class R2dbcBindNameCacheTest {

    @Test
    public void shouldReuseNameAcrossBindingKinds() {
        BindingExecutor executor = new BindingExecutor();
        List<String> names = new ArrayList<>();
        Statement statement = captureNames(names);
        R2dbcParameterBinder custom = (target, name) -> target.bind(name, 42);
        for (Object value : Arrays.asList(42, null, NullValue.of(Integer.class, null), custom)) {
            executor.prepareStatement(statement, SqlRequests.prepare("select $1", value));
        }
        assertEquals(Arrays.asList("$1", "$1", "$1", "$1"), names);
        for (String name : names) {
            assertSame(names.get(0), name);
        }
    }

    @Test
    public void shouldPreserveCacheBoundariesAndChangedConfiguration() {
        BindingExecutor executor = new BindingExecutor();
        List<String> names = new ArrayList<>();
        Statement statement = captureNames(names);
        int[] indexes = {0, 6399, 65_535, 65_536, -1};
        for (int index : indexes) {
            executor.bind(statement, index, 1);
            executor.bindNull(statement, index, Integer.class);
        }
        for (int i = 0; i < indexes.length; i++) {
            assertEquals("$" + (indexes[i] + 1), names.get(i * 2));
            assertEquals(names.get(i * 2), names.get(i * 2 + 1));
            if (indexes[i] >= 0 && indexes[i] < 65_536) {
                assertSame(names.get(i * 2), names.get(i * 2 + 1));
            }
        }
        names.clear();
        executor.symbol = "@arg";
        executor.firstIndex = 0;
        executor.bind(statement, 0, 1);
        executor.firstIndex = 1;
        executor.bindNull(statement, 0, Integer.class);
        executor.symbol = "$";
        executor.bind(statement, 0, 1);
        assertEquals(Arrays.asList("@arg0", "@arg1", "$1"), names);
    }

    @Test(timeout = 30_000)
    public void shouldBindCorrectNamesDuringConcurrentColdAndWarmAccess() throws Exception {
        BindingExecutor executor = new BindingExecutor();
        R2dbcParameterBinder custom = (target, name) -> target.bind(name, 42);
        Object[] values = new Object[6400];
        Object[] kinds = {42, null, NullValue.of(Integer.class, null), custom};
        for (int i = 0; i < values.length; i++) {
            values[i] = kinds[i % kinds.length];
        }
        SqlRequest request = SqlRequests.prepare("select $1", values);
        ExecutorService pool = Executors.newFixedThreadPool(6);
        CyclicBarrier start = new CyclicBarrier(6);
        List<Future<?>> results = new ArrayList<>();
        try {
            for (int thread = 0; thread < 6; thread++) {
                results.add(pool.submit(() -> {
                    int[] position = {0};
                    Statement statement = (Statement) Proxy.newProxyInstance(
                        Statement.class.getClassLoader(), new Class[]{Statement.class}, (proxy, method, args) -> {
                            assertEquals("$" + (++position[0]), args[0]);
                            return proxy;
                        });
                    start.await(10, TimeUnit.SECONDS);
                    for (int round = 0; round < 4; round++) {
                        position[0] = 0;
                        assertSame(statement, executor.prepareStatement(statement, request));
                        assertEquals(values.length, position[0]);
                    }
                    return null;
                }));
            }
            for (Future<?> result : results) {
                result.get(20, TimeUnit.SECONDS);
            }
        } finally {
            pool.shutdownNow();
            assertTrue(pool.awaitTermination(10, TimeUnit.SECONDS));
        }
    }

    private Statement captureNames(List<String> names) {
        return (Statement) Proxy.newProxyInstance(
            Statement.class.getClassLoader(), new Class[]{Statement.class}, (proxy, method, args) -> {
                names.add((String) args[0]);
                return proxy;
            });
    }

    private static class BindingExecutor extends R2dbcReactiveSqlExecutor {
        private String symbol = "$";
        private int firstIndex = 1;

        @Override
        protected String getBindSymbol() {
            return symbol;
        }

        @Override
        protected int getBindFirstIndex() {
            return firstIndex;
        }

        @Override
        protected Mono<Connection> getConnection() {
            return Mono.error(new UnsupportedOperationException());
        }

        @Override
        protected void releaseConnection(SignalType type, Connection connection) {
        }
    }
}
