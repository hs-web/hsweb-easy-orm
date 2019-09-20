package org.hswebframework.ezorm.spring.executor;

import io.r2dbc.spi.ConnectionFactories;
import io.r2dbc.spi.ConnectionFactory;
import io.r2dbc.spi.ConnectionFactoryOptions;
import lombok.SneakyThrows;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.transaction.TransactionAutoConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import java.net.URL;

import static io.r2dbc.spi.ConnectionFactoryOptions.*;

@SpringBootApplication(exclude = TransactionAutoConfiguration.class)
@EnableTransactionManagement
public class TestApplication {


    @Bean
    public TransactionR2dbcSqlExecutor r2dbcSqlExecutor(ConnectionFactory connectionFactory) {

        return new TransactionR2dbcSqlExecutor() {
            @Override
            protected ConnectionFactory getConnectionFactory() {
                return connectionFactory;
            }
        };
    }

}
