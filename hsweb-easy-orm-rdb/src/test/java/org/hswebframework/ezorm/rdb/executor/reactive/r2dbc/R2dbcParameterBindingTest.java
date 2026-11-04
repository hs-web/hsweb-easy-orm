package org.hswebframework.ezorm.rdb.executor.reactive.r2dbc;

import io.r2dbc.spi.Clob;
import io.r2dbc.spi.Connection;
import io.r2dbc.spi.Statement;
import org.hswebframework.ezorm.rdb.codec.LongCharSequence;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.R2dbcParameterBinder;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.junit.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;

import java.lang.reflect.Proxy;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import static org.junit.Assert.*;

public class R2dbcParameterBindingTest {

    @Test
    public void shouldPreserveTypesAndCustomBinder() {
        BindingExecutor executor = new BindingExecutor();
        List<List<Object>> calls = new ArrayList<>();
        Statement statement = (Statement) Proxy.newProxyInstance(
            Statement.class.getClassLoader(), new Class[]{Statement.class}, (proxy, method, args) -> {
                calls.add(Arrays.asList(method.getName(), args[0], args[1]));
                return proxy;
            });
        Date date = new Date(1712345678901L);
        R2dbcParameterBinder custom = (target, name) -> target.bind(name, "custom");
        SqlRequest request = SqlRequests.prepare(
            "insert into data values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
            42, 12.5D, true, "text", date, null, NullValue.of(Date.class, null),
            NullValue.of(LongCharSequence.class, null), new LongCharSequence("large"), custom);

        SqlRequest converted = executor.convertRequest(request);
        assertEquals("insert into data values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)", converted.getSql());
        assertSame(statement, executor.prepareStatement(statement, converted));
        assertEquals(Arrays.asList("bind", "$1", 42), calls.get(0));
        assertEquals(Arrays.asList("bind", "$2", 12.5D), calls.get(1));
        assertEquals(Arrays.asList("bind", "$3", true), calls.get(2));
        assertEquals(Arrays.asList("bind", "$4", "text"), calls.get(3));
        assertEquals(Arrays.asList("bind", "$5", LocalDateTime.ofInstant(date.toInstant(), ZoneId.systemDefault())), calls.get(4));
        assertEquals(Arrays.asList("bindNull", "$6", String.class), calls.get(5));
        assertEquals(Arrays.asList("bindNull", "$7", LocalDateTime.class), calls.get(6));
        assertEquals(Arrays.asList("bindNull", "$8", Clob.class), calls.get(7));
        assertEquals("$9", calls.get(8).get(1));
        Clob clob = (Clob) calls.get(8).get(2);
        try {
            assertEquals("large", Flux.from(clob.stream()).collectList().block().get(0).toString());
        } finally {
            Mono.from(clob.discard()).block();
        }
        assertEquals(Arrays.asList("bind", "$10", "custom"), calls.get(9));
        assertEquals(10, calls.size());
    }

    @Test
    public void shouldPreserveBindingHooksAndPlaceholderConfiguration() {
        List<Integer> indexes = new ArrayList<>();
        BindingExecutor executor = new BindingExecutor() {
            @Override
            protected String getBindSymbol() {
                return "@arg";
            }

            @Override
            protected int getBindFirstIndex() {
                return 0;
            }

            @Override
            protected void bind(Statement statement, int index, Object value) {
                indexes.add(index);
            }

            @Override
            protected void bindNull(Statement statement, int index, Class<?> type) {
                indexes.add(index);
            }
        };
        SqlRequest request = executor.convertRequest(SqlRequests.prepare("select ?, ?", 1, null));
        assertEquals("select @arg0, @arg1", request.getSql());
        executor.prepareStatement(null, request);
        assertEquals(Arrays.asList(0, 1), indexes);
    }

    private static class BindingExecutor extends R2dbcReactiveSqlExecutor {
        @Override
        protected Mono<Connection> getConnection() {
            return Mono.error(new UnsupportedOperationException());
        }

        @Override
        protected void releaseConnection(SignalType type, Connection connection) {
        }
    }
}
