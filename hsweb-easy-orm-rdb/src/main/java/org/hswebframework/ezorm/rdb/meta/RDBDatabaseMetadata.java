package org.hswebframework.ezorm.rdb.meta;

import org.hswebframework.ezorm.core.meta.AbstractDatabaseMetadata;
import org.hswebframework.ezorm.rdb.meta.dialect.Dialect;

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

    @Override
    public void addSchema(RDBSchemaMetadata schema) {
        schema.setDatabase(this);
        super.addSchema(schema);
    }
}
