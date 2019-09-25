package org.hswebframework.ezorm.spring.executor;

import io.r2dbc.spi.ConnectionFactory;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.EntityManager;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.mapping.jpa.JpaEntityTableMetadataParser;
import org.hswebframework.ezorm.rdb.mapping.parser.DataTypeResolver;
import org.hswebframework.ezorm.rdb.mapping.parser.DefaultDataTypeResolver;
import org.hswebframework.ezorm.rdb.mapping.parser.EntityTableMetadataParser;
import org.hswebframework.ezorm.rdb.mapping.parser.ValueCodecResolver;
import org.hswebframework.ezorm.rdb.mapping.wrapper.EntityResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.metadata.parser.TableMetadataParser;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.supports.posgres.PostgresqlSchemaMetadata;
import org.hswebframework.ezorm.spring.CompositeEntityTableMetadataResolver;
import org.hswebframework.ezorm.spring.annotation.EnableEasyormRepository;
import org.hswebframework.ezorm.spring.EntityResultWrapperFactory;
import org.hswebframework.ezorm.spring.EntityTableMetadataResolver;
import org.hswebframework.ezorm.spring.mapping.JpaEntityTableMetadataResolver;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.transaction.TransactionAutoConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;

@SpringBootApplication(exclude = TransactionAutoConfiguration.class, scanBasePackages = "org.hswebframework.ezorm.spring")
@EnableTransactionManagement
@EnableEasyormRepository(value = "org.hswebframework.ezorm.spring.mapping", enableReactive = true)
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
    public ReactiveSyncSqlExecutor syncSqlExecutor(ReactiveSqlExecutor sqlExecutor) {
        return ReactiveSyncSqlExecutor.of(sqlExecutor);
    }

    @Bean
    public DatabaseOperator databaseOperator(ReactiveSqlExecutor sqlExecutor, SyncSqlExecutor syncSqlExecutor) {
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.POSTGRES);

        database.addFeature(sqlExecutor);
        database.addFeature(syncSqlExecutor);

        PostgresqlSchemaMetadata publicSchema = new PostgresqlSchemaMetadata("public");
        database.setCurrentSchema(publicSchema);
        database.addSchema(publicSchema);

        return DefaultDatabaseOperator.of(database);
    }

    @Bean
    public EntityTableMetadataParser jpaEntityTableMetadataParser(DatabaseOperator operator,
                                                                  Optional<DataTypeResolver> resolver,
                                                                  Optional<ValueCodecResolver> codecResolver
                                                                  ) {
        JpaEntityTableMetadataParser parser = new JpaEntityTableMetadataParser();
        parser.setDatabaseMetadata(operator.getMetadata());

        resolver.ifPresent(parser::setDataTypeResolver);
        codecResolver.ifPresent(parser::setValueCodecResolver);

        return parser;
    }

    @Bean
    public EntityTableMetadataResolver entityTableMappingResolver(List<EntityTableMetadataParser> parsers) {
        CompositeEntityTableMetadataResolver resolver = new CompositeEntityTableMetadataResolver();
        parsers.forEach(resolver::addParser);
        return resolver;
    }

    @Bean
    public EntityManager entityManager(EntityTableMetadataResolver resolver) {
        return new EntityManager() {
            @Override
            @SneakyThrows
            public <E> E newInstance(Class<E> type) {
                return type.newInstance();
            }

            @Override
            public EntityColumnMapping getMapping(Class entity) {

                return resolver.resolve(entity).getFeature(MappingFeatureType.columnPropertyMapping.createFeatureId(entity))
                        .map(EntityColumnMapping.class::cast)
                        .orElse(null);
            }
        };
    }

    @Bean
    public EntityResultWrapperFactory entityResultWrapperFactory() {
        return new EntityResultWrapperFactory() {
            @Override
            public <T> ResultWrapper<T, T> getWrapper(Class<T> tClass) {

                return new EntityResultWrapper<>(new Supplier() {
                    @Override
                    @SneakyThrows
                    public T get() {
                        return (T) tClass.newInstance();
                    }
                });
            }
        };
    }

}
