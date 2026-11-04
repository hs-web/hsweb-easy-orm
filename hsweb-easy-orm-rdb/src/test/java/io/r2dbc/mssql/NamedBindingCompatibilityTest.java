package io.r2dbc.mssql;

import io.r2dbc.spi.Connection;
import io.r2dbc.spi.Statement;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.R2dbcParameterBinder;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.reactive.r2dbc.R2dbcReactiveSqlExecutor;
import org.junit.Test;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;

import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;

/**
 * Verifies executor binding targets with the real SQL Server driver parser.
 * Lives beside the driver's package-private parser only in test scope; no server or copied parser is used.
 */
public class NamedBindingCompatibilityTest {

    @Test
    public void shouldPreserveOrderedNames() {
        checkBindings("select @arg1 as a, @arg2 as b", 1);
    }

    @Test
    public void shouldPreserveReorderedNames() {
        checkBindings("select @arg2 as a, @arg1 as b", 1);
    }

    @Test
    public void shouldPreserveRepeatedNames() {
        checkBindings("select @arg1 as a, @arg1 as b, @arg2 as c", 1);
    }

    @Test
    public void shouldPreserveZeroBasedNameNumbering() {
        checkBindings("select @arg1 as a, @arg0 as b, @arg0 as c", 0);
    }

    private void checkBindings(String sql, int firstIndex) {
        R2dbcParameterBinder custom = (target, name) -> target.bind(name, "custom");
        for (Object[] parameters : Arrays.asList(
            new Object[]{11, 22},
            new Object[]{null, 22},
            new Object[]{11, null},
            new Object[]{NullValue.of(Integer.class, null), 22},
            new Object[]{11, NullValue.of(Long.class, null)},
            new Object[]{custom, 22},
            new Object[]{11, custom},
            new Object[]{custom, NullValue.of(Integer.class, null)})) {
            List<String> targets = new ArrayList<>();
            ParametrizedMssqlStatement.ParsedQuery parsed = ParametrizedMssqlStatement.ParsedQuery.parse(sql);
            Statement statement = (Statement) Proxy.newProxyInstance(
                Statement.class.getClassLoader(), new Class[]{Statement.class}, (proxy, method, args) -> {
                    targets.add(args[0] instanceof Integer
                                    ? parsed.getParameterName((Integer) args[0])
                                    : parsed.getParameterName((String) args[0]));
                    return proxy;
                });
            BindingExecutor executor = new BindingExecutor(firstIndex);
            SqlRequest request = executor.convert(SqlRequests.prepare(sql, parameters));
            assertEquals(sql, request.getSql());
            assertSame(statement, executor.prepare(statement, request));
            assertEquals(sql, Arrays.asList("arg" + firstIndex, "arg" + (firstIndex + 1)), targets);
        }
    }

    private static class BindingExecutor extends R2dbcReactiveSqlExecutor {
        private final int firstIndex;

        private BindingExecutor(int firstIndex) {
            this.firstIndex = firstIndex;
        }

        SqlRequest convert(SqlRequest request) {
            return convertRequest(request);
        }

        Statement prepare(Statement statement, SqlRequest request) {
            return prepareStatement(statement, request);
        }

        @Override
        protected String getBindSymbol() {
            return "@arg";
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
