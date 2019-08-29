package org.hswebframework.ezorm.core.meta;

import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.BiFunction;

import static java.util.Optional.*;

public abstract class AbstractDatabaseMetadata<S extends SchemaMetadata> implements DatabaseMetadata<S> {

    private Map<String, S> schemas = new ConcurrentHashMap<>();

    @Getter
    @Setter
    private S currentSchema;

    @Getter
    @Setter
    protected String databaseName;

    @Getter
    @Setter
    private String name;

    @Getter
    @Setter
    private String alias;

    public void addSchema(S schema) {
        schemas.put(schema.getName(), schema);
        if (schema.getAlias() != null) {
            schemas.put(schema.getAlias(), schema);
        }
    }

    @Override
    public List<S> getSchemas() {
        return new ArrayList<>(schemas.values());
    }

    @Override
    public Optional<S> getSchema(String name) {
        return ofNullable(schemas.get(name));
    }

    @Override
    public <T extends ObjectMetadata> Optional<T> getObject(String name, BiFunction<S, String, Optional<T>> mapper) {
        if (name == null) {
            return empty();
        }
        if (name.contains(".")) {
            String[] arr = name.split(".");
            return this.getSchema(arr[0])
                    .flatMap(schema -> mapper.apply(schema, arr[1]));
        }
        return of(this.getCurrentSchema())
                .flatMap(schema -> mapper.apply(schema, name));
    }
}
