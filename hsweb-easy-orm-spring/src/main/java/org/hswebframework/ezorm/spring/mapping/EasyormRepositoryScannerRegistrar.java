package org.hswebframework.ezorm.spring.mapping;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.rdb.mapping.ReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.defaults.DefaultReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.jpa.JpaEntityTableMetadataParser;
import org.hswebframework.ezorm.rdb.mapping.wrapper.EntityResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.supports.posgres.PostgresqlSchemaMetadata;
import org.hswebframework.ezorm.spring.annotation.EnableEasyormRepository;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.GenericBeanDefinition;
import org.springframework.beans.factory.support.RootBeanDefinition;
import org.springframework.context.PayloadApplicationEvent;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.core.ResolvableType;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;
import org.springframework.core.io.support.ResourcePatternResolver;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.core.type.classreading.CachingMetadataReaderFactory;
import org.springframework.core.type.classreading.MetadataReader;
import org.springframework.core.type.classreading.MetadataReaderFactory;

import java.util.Map;
import java.util.function.Supplier;

@Slf4j
public class EasyormRepositoryScannerRegistrar implements ImportBeanDefinitionRegistrar, BeanFactoryAware {

    private ResourcePatternResolver resourcePatternResolver = new PathMatchingResourcePatternResolver();

    @Autowired
    private MetadataReaderFactory metadataReaderFactory = new CachingMetadataReaderFactory();

    @Autowired
    private DatabaseOperator databaseOperator;

    @Autowired
    private EntityResultWrapperFactory wrapperFactory;

    private BeanFactory beanFactory;

    @Override
    @SneakyThrows
    public void registerBeanDefinitions(AnnotationMetadata importingClassMetadata, BeanDefinitionRegistry registry) {

//        Map<String, Object> attr = importingClassMetadata.getAnnotationAttributes(EnableEasyormRepository.class.getName());
//        String[] arr =  (String[]) attr.get("value");

        for (Resource resource : resourcePatternResolver.getResources("classpath*:/org/hswebframework/**/*Entity.class")) {
            MetadataReader reader = metadataReaderFactory.getMetadataReader(resource);
            String className = reader.getClassMetadata().getClassName();
            Class entityType = Class.forName(className);


            ResolvableType repositoryType = ResolvableType.forClassWithGenerics(DefaultReactiveRepository.class, entityType, String.class);



            JpaEntityTableMetadataParser parser=new JpaEntityTableMetadataParser();
            parser.setDatabaseMetadata(getDatabase().getMetadata());;

            parser.parseTable(entityType).
                    ifPresent(table->{
                        RootBeanDefinition definition = new RootBeanDefinition();
                        definition.setTargetType(repositoryType);
                        definition.setBeanClass(DefaultReactiveRepository.class);

                        definition.setAutowireMode(AbstractBeanDefinition.AUTOWIRE_BY_TYPE);
                        definition.getConstructorArgumentValues()
                                .addIndexedArgumentValue(0, databaseOperator);
                        definition.getConstructorArgumentValues()
                                .addIndexedArgumentValue(1,table);

                        definition.getConstructorArgumentValues()
                                .addIndexedArgumentValue(2,entityType);
                        definition.getConstructorArgumentValues()
                                .addIndexedArgumentValue(3,new EntityResultWrapper<>(new Supplier<Object>() {
                                    @Override
                                    @SneakyThrows
                                    public Object get() {
                                        return entityType.newInstance();
                                    }
                                }));

                        registry.registerBeanDefinition(entityType.getName().concat("Repository"), definition);

                    });


        }

    }

    public DatabaseOperator getDatabase(){
        if(databaseOperator!=null){
            return databaseOperator;
        }
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.POSTGRES);

//        database.addFeature(sqlExecutor);

        PostgresqlSchemaMetadata publicSchema = new PostgresqlSchemaMetadata("public");
        database.setCurrentSchema(publicSchema);
        database.addSchema(publicSchema);

        return databaseOperator =DefaultDatabaseOperator.of(database);
    }

    @Override
    public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
        this.beanFactory = beanFactory;
//        this.databaseOperator=beanFactory.getBean(DatabaseOperator.class);
//        this.wrapperFactory=beanFactory.getBean(EntityResultWrapperFactory.class);

    }
}
