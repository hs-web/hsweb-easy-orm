package org.hswebframework.ezorm.core.meta;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.ObjectWrapperFactory;
import org.hswebframework.ezorm.core.ValidatorFactory;
import org.hswebframework.ezorm.core.meta.storage.MapTableMetaDataStorage;
import org.hswebframework.ezorm.core.meta.storage.TableMetaDataStorage;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

public abstract class AbstractDatabaseMetaData implements DatabaseMetaData {

    private Map<String, SchemaMetaData> schemas = new ConcurrentHashMap<>();

    @Getter
    @Setter
    private SchemaMetaData currentSchema;

    @Getter
    @Setter
    protected String databaseName;

    @Getter
    @Setter
    private String name;

    @Getter
    @Setter
    private String alias;

    public void registerSchema(SchemaMetaData schema) {
        schemas.put(schema.getName(), schema);
        if (schema.getAlias() != null) {
            schemas.put(schema.getAlias(), schema);
        }
    }

    @Override
    public List<SchemaMetaData> getSchemas() {
        return new ArrayList<>(schemas.values());
    }

    @Override
    public Optional<SchemaMetaData> getSchema(String name) {
        return Optional.ofNullable(schemas.get(name));
    }
}
