package org.hswebframework.ezorm.spring;

import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;

public class CompositeEntityTableMetadataResolver implements EntityTableMetadataResolver {

    private List<EntityTableMetadataResolver> resolvers = new ArrayList<>();

    private Map<Class, AtomicReference<RDBTableMetadata>> cache = new ConcurrentHashMap<>();

    public void addResolver(EntityTableMetadataResolver resolver) {
        resolvers.add(resolver);
    }

    @Override
    public RDBTableMetadata resolve(Class<?> entityClass) {

        return cache.computeIfAbsent(entityClass, type -> new AtomicReference<>(doResolve(type))).get();
    }

    private RDBTableMetadata doResolve(Class<?> entityClass) {
        return resolvers.stream()
                .map(resolver -> resolver.resolve(entityClass))
                .filter(Objects::nonNull)
                .reduce((t1, t2) -> {
                    for (RDBColumnMetadata column : t1.getColumns()) {
                        t2.addColumn(column.clone());
                    }
                    return t2;
                }).orElse(null);
    }
}
