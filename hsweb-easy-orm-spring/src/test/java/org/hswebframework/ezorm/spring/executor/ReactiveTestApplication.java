package org.hswebframework.ezorm.spring.executor;

import io.r2dbc.spi.ConnectionFactory;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.jpa.JpaEntityTableMetadataParser;
import org.hswebframework.ezorm.rdb.mapping.wrapper.EntityResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.supports.posgres.PostgresqlSchemaMetadata;
import org.hswebframework.ezorm.spring.annotation.EnableEasyormRepository;
import org.hswebframework.ezorm.spring.mapping.EntityResultWrapperFactory;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.transaction.TransactionAutoConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import java.util.function.Supplier;

@SpringBootApplication(exclude = TransactionAutoConfiguration.class)
@EnableTransactionManagement
@EnableEasyormRepository("")
public class ReactiveTestApplication {


    @Bean
    public TransactionR2dbcSqlExecutor r2dbcSqlExecutor(ConnectionFactory connectionFactory) {

        return new TransactionR2dbcSqlExecutor() {
            @Override
            protected ConnectionFactory getConnectionFactory() {
                return connectionFactory;
            }
        };
    }

    @Bean
    public DatabaseOperator databaseOperator(ReactiveSqlExecutor sqlExecutor){
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.POSTGRES);

        database.addFeature(sqlExecutor);

        PostgresqlSchemaMetadata publicSchema = new PostgresqlSchemaMetadata("public");
        database.setCurrentSchema(publicSchema);
        database.addSchema(publicSchema);

        return DefaultDatabaseOperator.of(database);
    }

    @Bean
    public EntityResultWrapperFactory entityResultWrapperFactory(){
        return new EntityResultWrapperFactory() {
            @Override
            public <T> ResultWrapper<T, T> getWrapper(Class<T> tClass) {

                return new EntityResultWrapper<>(new Supplier() {
                    @Override
                    @SneakyThrows
                    public T get() {
                        return (T)tClass.newInstance();
                    }
                });
            }
        }  ;
    }

}
