package org.hswebframework.ezorm.spring.mapping;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.rdb.mapping.EntityManager;
import org.hswebframework.ezorm.rdb.mapping.SyncRepository;
import org.hswebframework.ezorm.rdb.mapping.defaults.DefaultSyncRepository;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.spring.EntityResultWrapperFactory;
import org.hswebframework.ezorm.spring.EntityTableMetadataResolver;
import org.springframework.beans.factory.FactoryBean;

@Getter
@Setter
public class SyncRepositoryFactoryBean<E, PK>
        implements FactoryBean<SyncRepository<E, PK>> {

    private DatabaseOperator operator;

    private EntityTableMetadataResolver mappingResolver;

    private Class<E> entityType;

    private EntityManager entityManager;

    private EntityResultWrapperFactory wrapperFactory;

    @Override
    public SyncRepository<E, PK> getObject() {

        return new DefaultSyncRepository<>(operator,
                mappingResolver.resolve(entityType),
                entityType,
                wrapperFactory.getWrapper(entityType));
    }

    @Override
    public Class<?> getObjectType() {
        return SyncRepository.class;
    }
}
