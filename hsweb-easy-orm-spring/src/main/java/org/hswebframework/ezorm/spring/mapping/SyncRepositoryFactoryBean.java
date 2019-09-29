package org.hswebframework.ezorm.spring.mapping;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.rdb.mapping.EntityManager;
import org.hswebframework.ezorm.rdb.mapping.SyncRepository;
import org.hswebframework.ezorm.rdb.mapping.defaults.DefaultSyncRepository;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.spring.EntityResultWrapperFactory;
import org.hswebframework.ezorm.spring.EntityTableMetadataResolver;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.annotation.Autowired;

@Getter
@Setter
public class SyncRepositoryFactoryBean<E, PK>
        implements FactoryBean<SyncRepository<E, PK>> {


    @Autowired
    private DatabaseOperator operator;


    @Autowired
    private EntityTableMetadataResolver resolver;

    private Class<E> entityType;

    @Autowired
    private EntityResultWrapperFactory wrapperFactory;

    @Override
    public SyncRepository<E, PK> getObject() {
        RDBTableMetadata table = resolver.resolve(entityType);

        return new DefaultSyncRepository<>(operator,
                table.getFullName(),
                entityType,
                wrapperFactory.getWrapper(entityType));
    }

    @Override
    public Class<?> getObjectType() {
        return SyncRepository.class;
    }
}
