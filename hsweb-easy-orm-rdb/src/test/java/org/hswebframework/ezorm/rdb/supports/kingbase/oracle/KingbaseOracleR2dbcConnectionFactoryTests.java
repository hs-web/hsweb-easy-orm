package org.hswebframework.ezorm.rdb.supports.kingbase.oracle;

import io.r2dbc.spi.Connection;
import io.r2dbc.spi.ConnectionFactories;
import io.r2dbc.spi.ConnectionFactory;
import io.r2dbc.spi.ConnectionFactoryOptions;
import io.r2dbc.spi.R2dbcNonTransientResourceException;
import org.hswebframework.ezorm.rdb.supports.kingbase.KingbaseTestContainer;
import org.junit.Assert;
import org.junit.Assume;
import org.junit.ClassRule;
import org.junit.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.io.IOException;
import java.net.ConnectException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.time.Duration;

import static io.r2dbc.spi.ConnectionFactoryOptions.CONNECT_TIMEOUT;
import static io.r2dbc.spi.ConnectionFactoryOptions.PASSWORD;
import static io.r2dbc.spi.ConnectionFactoryOptions.USER;

public class KingbaseOracleR2dbcConnectionFactoryTests {

    @ClassRule
    public static final KingbaseTestContainer DATABASE = new KingbaseTestContainer("oracle");

    private static final Duration TIMEOUT = Duration.ofSeconds(20);

    @Test
    public void shouldCreateConnectionAndUseOracleCompatibilityMode() {
        Assume.assumeTrue(
            "Set -Dkingbase.oracle.r2dbc.integration=true to run the local Kingbase Oracle integration test",
            Boolean.getBoolean("kingbase.oracle.r2dbc.integration")
                || Boolean.getBoolean("kingbase.oracle.r2dbc.enabled")
        );

        String password = configuration(
            "kingbase.oracle.r2dbc.password",
            "KINGBASE_ORACLE_R2DBC_PASSWORD",
            null
        );
        Assume.assumeTrue(
            "Set kingbase.oracle.r2dbc.password or KINGBASE_ORACLE_R2DBC_PASSWORD to run the local integration test",
            password != null && !password.isEmpty()
        );

        ConnectionFactory connectionFactory = ConnectionFactories.get(
            ConnectionFactoryOptions.parse(configuration(
                "kingbase.oracle.r2dbc.url",
                "KINGBASE_ORACLE_R2DBC_URL",
                "r2dbc:kingbase://127.0.0.1:54322/test?mode=oracle"
            )).mutate()
              .option(USER, configuration("kingbase.oracle.r2dbc.username", "KINGBASE_ORACLE_R2DBC_USERNAME", "system"))
              .option(PASSWORD, password)
              .build()
        );
        Assert.assertEquals("KingbaseES", connectionFactory.getMetadata().getName());

        Connection connection = Mono.from(connectionFactory.create()).block(TIMEOUT);
        Assert.assertNotNull(connection);

        try {
            String mode = Flux.from(connection.createStatement("show database_mode").execute())
                .flatMap(result -> result.map((row, metadata) -> row.get(0, String.class)))
                .single()
                .block(TIMEOUT);

            Assert.assertEquals("oracle", mode == null ? null : mode.toLowerCase());

            Integer answer = Flux.from(connection.createStatement("select $1 as answer from dual")
                    .bind("$1", 42)
                    .execute())
                .flatMap(result -> result.map((row, metadata) -> row.get("answer", Integer.class)))
                .single()
                .block(TIMEOUT);

            Assert.assertEquals(Integer.valueOf(42), answer);
        } finally {
            Mono.from(connection.close()).block(TIMEOUT);
        }
    }

    @Test
    public void shouldFailWhenConnectionPortIsNotListening() throws IOException {
        // Reserve a local TCP port without listening, so no database is needed.
        try (Socket endpoint = new Socket()) {
            endpoint.bind(new InetSocketAddress("127.0.0.1", 0));
            String url = "r2dbc:kingbase://127.0.0.1:" + endpoint.getLocalPort() + "/test?mode=oracle";
            ConnectionFactoryOptions options = ConnectionFactoryOptions
                .parse(url)
                .mutate()
                .option(USER, "connection-test")
                .option(PASSWORD, "unused")
                .option(CONNECT_TIMEOUT, Duration.ofSeconds(2))
                .build();
            Assume.assumeTrue("Enable -Pkingbase-r2dbc to load the Kingbase driver",
                              ConnectionFactories.supports(options));
            ConnectionFactory factory = ConnectionFactories.get(options);
            Assert.assertEquals("KingbaseES", factory.getMetadata().getName());
            System.out.println("[连接异常测试] 模式=oracle，地址=" + url);
            System.out.println("[连接工厂] 创建成功，驱动=" + factory.getMetadata().getName());
            System.out.println("[预期结果] 目标端口未监听，建立连接时应报告连接被拒绝");

            StepVerifier.create(Mono.from(factory.create()))
                .expectErrorSatisfies(error -> {
                    System.out.println("[实际异常] " + error.getClass().getName() + ": " + error.getMessage());
                    Assert.assertTrue("Expected an R2DBC connection failure: " + error,
                                      error instanceof R2dbcNonTransientResourceException);
                    R2dbcNonTransientResourceException exception = (R2dbcNonTransientResourceException) error;
                    System.out.println("[错误标识] SQLState=" + exception.getSqlState()
                        + "，错误码=" + exception.getErrorCode());
                    System.out.println("[底层原因] " + error.getCause());
                    Assert.assertTrue("Expected connection refusal: " + error.getCause(),
                                      error.getCause() instanceof ConnectException);
                })
                .verify(TIMEOUT);
            System.out.println("[验证结果] 已捕获预期的连接拒绝异常，测试通过");
        }
    }

    private static String configuration(String property, String environment, String defaultValue) {
        String value = System.getProperty(property);
        if (value == null || value.isEmpty()) {
            value = System.getenv(environment);
        }
        return value == null || value.isEmpty() ? defaultValue : value;
    }
}
