package org.hswebframework.ezorm.rdb.metadata;

import org.hswebframework.ezorm.core.meta.AbstractDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;

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

    public Optional<RDBTableMetadata> getTable(String name) {
        return this.getObject(name, RDBSchemaMetadata::getTable);
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
