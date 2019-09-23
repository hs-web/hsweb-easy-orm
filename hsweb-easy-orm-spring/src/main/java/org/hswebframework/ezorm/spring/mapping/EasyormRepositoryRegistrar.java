package org.hswebframework.ezorm.spring.mapping;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.rdb.mapping.EntityManager;
import org.hswebframework.ezorm.rdb.mapping.defaults.DefaultReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.defaults.DefaultSyncRepository;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.spring.EntityResultWrapperFactory;
import org.hswebframework.ezorm.spring.EntityTableMetadataResolver;
import org.hswebframework.ezorm.spring.annotation.EnableEasyormRepository;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.RootBeanDefinition;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.core.ResolvableType;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;
import org.springframework.core.io.support.ResourcePatternResolver;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.core.type.classreading.MetadataReader;
import org.springframework.core.type.classreading.MetadataReaderFactory;
import org.springframework.core.type.classreading.SimpleMetadataReaderFactory;

import java.lang.annotation.Annotation;
import java.util.Arrays;
import java.util.Map;
import java.util.stream.Collectors;

@Slf4j
public class EasyormRepositoryRegistrar implements ImportBeanDefinitionRegistrar {

    private ResourcePatternResolver resourcePatternResolver = new PathMatchingResourcePatternResolver();

    private MetadataReaderFactory metadataReaderFactory = new SimpleMetadataReaderFactory();

    @Override
    @SneakyThrows
    @SuppressWarnings("all")
    public void registerBeanDefinitions(AnnotationMetadata importingClassMetadata, BeanDefinitionRegistry registry) {

        Map<String, Object> attr = importingClassMetadata.getAnnotationAttributes(EnableEasyormRepository.class.getName());
        if (attr == null) {
            return;
        }
        String[] arr = (String[]) attr.get("value");
        String path = Arrays.stream(arr)
                .map(str -> ResourcePatternResolver
                        .CLASSPATH_ALL_URL_PREFIX
                        .concat(str.replace(".", "/")).concat("/**/*.class"))
                .collect(Collectors.joining());

        Class<Annotation>[] anno = (Class[]) attr.get("annotation");
        boolean enableSync = attr.containsKey("enableSync");
        boolean enableReactive = attr.containsKey("enableReactive");

        for (Resource resource : resourcePatternResolver.getResources(path)) {
            MetadataReader reader = metadataReaderFactory.getMetadataReader(resource);
            String className = reader.getClassMetadata().getClassName();
            Class entityType = Class.forName(className);
            if (Arrays.stream(anno)
                    .noneMatch(ann -> AnnotationUtils.findAnnotation(entityType, ann) != null)) {
                continue;
            }

            if (enableReactive) {
                ResolvableType repositoryType = ResolvableType.forClassWithGenerics(DefaultReactiveRepository.class, entityType, String.class);

                log.debug("register easyorm reactive repository for {}", entityType);

                RootBeanDefinition definition = new RootBeanDefinition();
                definition.setTargetType(repositoryType);
                definition.setBeanClass(ReactiveRepositoryFactoryBean.class);
                definition.setAutowireMode(AbstractBeanDefinition.AUTOWIRE_BY_TYPE);
                definition.getPropertyValues().add("operator", new RuntimeBeanReference(DatabaseOperator.class));
                definition.getPropertyValues().add("mappingResolver", new RuntimeBeanReference(EntityTableMetadataResolver.class));
                definition.getPropertyValues().add("entityType", entityType);
                definition.getPropertyValues().add("entityManager", new RuntimeBeanReference(EntityManager.class));
                definition.getPropertyValues().add("wrapperFactory", new RuntimeBeanReference(EntityResultWrapperFactory.class));
                registry.registerBeanDefinition(entityType.getSimpleName().concat("ReactiveRepository"), definition);
            }

            if (enableSync) {
                ResolvableType repositoryType = ResolvableType.forClassWithGenerics(DefaultSyncRepository.class, entityType, String.class);

                log.debug("register easyorm synchronous repository for {}", entityType);

                RootBeanDefinition definition = new RootBeanDefinition();
                definition.setTargetType(repositoryType);
                definition.setBeanClass(SyncRepositoryFactoryBean.class);
                definition.setAutowireMode(AbstractBeanDefinition.AUTOWIRE_BY_TYPE);
                definition.getPropertyValues().add("operator", new RuntimeBeanReference(DatabaseOperator.class));
                definition.getPropertyValues().add("mappingResolver", new RuntimeBeanReference(EntityTableMetadataResolver.class));
                definition.getPropertyValues().add("entityType", entityType);
                definition.getPropertyValues().add("entityManager", new RuntimeBeanReference(EntityManager.class));
                definition.getPropertyValues().add("wrapperFactory", new RuntimeBeanReference(EntityResultWrapperFactory.class));
                registry.registerBeanDefinition(entityType.getSimpleName().concat("SyncRepository"), definition);
            }


        }

    }


}
