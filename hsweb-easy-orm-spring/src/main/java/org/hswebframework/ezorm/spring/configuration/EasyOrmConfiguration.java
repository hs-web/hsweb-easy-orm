package org.hswebframework.ezorm.spring.configuration;


import lombok.SneakyThrows;
import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.EntityManager;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.mapping.jpa.JpaEntityTableMetadataParser;
import org.hswebframework.ezorm.rdb.mapping.parser.DataTypeResolver;
import org.hswebframework.ezorm.rdb.mapping.parser.EntityTableMetadataParser;
import org.hswebframework.ezorm.rdb.mapping.parser.ValueCodecResolver;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.spring.CompositeEntityTableMetadataResolver;
import org.hswebframework.ezorm.spring.DefaultEntityResultWrapperFactory;
import org.hswebframework.ezorm.spring.EasyormProperties;
import org.hswebframework.ezorm.spring.EntityTableMetadataResolver;
import org.hswebframework.ezorm.spring.executor.DataSourceJdbcSyncSqlExecutor;
import org.hswebframework.ezorm.spring.executor.TransactionR2dbcSqlExecutor;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.r2dbc.ConnectionFactoryAutoConfiguration;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import javax.sql.DataSource;
import java.util.List;
import java.util.Optional;

@Configuration
@EnableConfigurationProperties(EasyormProperties.class)
public class EasyOrmConfiguration {

    @Autowired
    private EasyormProperties properties;

    @Bean
    public ReactiveSqlExecutor reactiveSqlExecutor(io.r2dbc.spi.ConnectionFactory factory) {
        return new TransactionR2dbcSqlExecutor() {
            @Override
            protected io.r2dbc.spi.ConnectionFactory getConnectionFactory() {
                return factory;
            }
        };
    }

    @Bean
    public SyncSqlExecutor reactiveSyncSqlExecutor(ReactiveSqlExecutor executor) {
        return ReactiveSyncSqlExecutor.of(executor);
    }

//    @Bean
//    @ConditionalOnBean(DataSource.class)
//    public SyncSqlExecutor syncSqlExecutor(DataSource dataSource) {
//        return new DataSourceJdbcSyncSqlExecutor() {
//            @Override
//            protected DataSource getDataSource() {
//                return dataSource;
//            }
//        };
//    }

    @Bean
    @ConditionalOnMissingBean
    public EntityManager entityManager(EntityTableMetadataResolver resolver) {
        return new EntityManager() {
            @Override
            @SneakyThrows
            public <E> E newInstance(Class<E> type) {
                return type.newInstance();
            }

            @Override
            public EntityColumnMapping getMapping(Class entity) {

                return resolver.resolve(entity)
                        .getFeature(MappingFeatureType.columnPropertyMapping.createFeatureId(entity))
                        .map(EntityColumnMapping.class::cast)
                        .orElse(null);
            }
        };
    }

    @Bean
    public DefaultEntityResultWrapperFactory defaultEntityResultWrapperFactory(EntityManager entityManager) {
        return new DefaultEntityResultWrapperFactory(entityManager);
    }

    @Bean
    @ConditionalOnMissingBean
    public EntityTableMetadataResolver entityTableMappingResolver(List<EntityTableMetadataParser> parsers) {
        CompositeEntityTableMetadataResolver resolver = new CompositeEntityTableMetadataResolver();
        parsers.forEach(resolver::addParser);
        return resolver;
    }

    @Bean
    @ConditionalOnMissingBean
    public EntityTableMetadataParser jpaEntityTableMetadataParser(DatabaseOperator operator,
                                                                  Optional<DataTypeResolver> resolver,
                                                                  Optional<ValueCodecResolver> codecResolver) {
        JpaEntityTableMetadataParser parser = new JpaEntityTableMetadataParser();
        parser.setDatabaseMetadata(operator.getMetadata());

        resolver.ifPresent(parser::setDataTypeResolver);
        codecResolver.ifPresent(parser::setValueCodecResolver);

        return parser;
    }

    @Bean
    @ConditionalOnMissingBean
    public DatabaseOperator databaseOperator() {
        RDBDatabaseMetadata metadata = properties.createDatabaseMetadata();

        return DefaultDatabaseOperator.of(metadata);
    }

    @Bean
    public BeanPostProcessor autoRegisterFeature(DatabaseOperator operator) {
        return new BeanPostProcessor() {
            @Override
            public Object postProcessAfterInitialization(Object bean, String beanName) throws BeansException {
                if (bean instanceof Feature) {
                    operator.getMetadata().addFeature(((Feature) bean));
                }
                return bean;
            }
        };
    }

}
