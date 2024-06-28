package org.hswebframework.ezorm.rdb.metadata;

import org.hswebframework.ezorm.core.meta.AbstractDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import reactor.core.publisher.Mono;

import java.util.Optional;

public class RDBDatabaseMetadata extends AbstractDatabaseMetadata<RDBSchemaMetadata> {
    protected Dialect dialect;

    public RDBDatabaseMetadata(Dialect dialect) {
        this.dialect = dialect;
    }

    public Dialect getDialect() {
        return dialect;
    }

    public Optional<TableOrViewMetadata> getTableOrView(String name) {
        return this.getObject(name, RDBSchemaMetadata::getTableOrView);
    }

    public Optional<TableOrViewMetadata> getTableOrView(String name, boolean autoLoad) {
        return this.getObject(name, (schema, _name) -> schema.getTableOrView(_name, autoLoad));
    }

    public Optional<RDBTableMetadata> getTable(String name) {
        return this.getObject(name, RDBSchemaMetadata::getTable);
    }

    public Mono<TableOrViewMetadata> getTableOrViewReactive(String name) {
        return this.getObjectReactive(name, RDBSchemaMetadata::getTableOrViewReactive);
    }

    public Mono<TableOrViewMetadata> getTableOrViewReactive(String name, boolean autoLoad) {
        return this.getObjectReactive(name, (schema, _name) -> schema.getTableOrViewReactive(_name, autoLoad));
    }

    public Mono<RDBTableMetadata> getTableReactive(String name) {
        return this.getObjectReactive(name, RDBSchemaMetadata::getTableReactive);
    }

    @Override
    public Optional<RDBSchemaMetadata> getSchema(String name) {
        return super.getSchema(getDialect().clearQuote(name));
    }

    @Override
    public void addSchema(RDBSchemaMetadata schema) {
        schema.setDatabase(this);
        super.addSchema(schema);
    }
}
