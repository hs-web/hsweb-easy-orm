package org.hswebframework.ezorm.rdb.supports.kingbase.oracle;

import io.r2dbc.spi.Connection;
import io.r2dbc.spi.ConnectionFactories;
import io.r2dbc.spi.ConnectionFactory;
import io.r2dbc.spi.ConnectionFactoryOptions;
import org.junit.Assert;
import org.junit.Assume;
import org.junit.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.Duration;

import static io.r2dbc.spi.ConnectionFactoryOptions.PASSWORD;
import static io.r2dbc.spi.ConnectionFactoryOptions.USER;

public class KingbaseOracleR2dbcConnectionFactoryTests {

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

    private static String configuration(String property, String environment, String defaultValue) {
        String value = System.getProperty(property);
        if (value == null || value.isEmpty()) {
            value = System.getenv(environment);
        }
        return value == null || value.isEmpty() ? defaultValue : value;
    }
}
