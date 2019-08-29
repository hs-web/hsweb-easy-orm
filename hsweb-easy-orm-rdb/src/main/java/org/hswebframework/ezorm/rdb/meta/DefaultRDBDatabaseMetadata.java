package org.hswebframework.ezorm.rdb.meta;

import org.hswebframework.ezorm.core.meta.AbstractDatabaseMetadata;
import org.hswebframework.ezorm.rdb.dialect.Dialect;

import java.util.Optional;

public class DefaultRDBDatabaseMetadata<S extends DefaultRDBSchemaMetadata> extends AbstractDatabaseMetadata<S> {
    protected Dialect dialect;

    public DefaultRDBDatabaseMetadata(Dialect dialect) {
        this.dialect = dialect;
    }

    public Dialect getDialect() {
        return dialect;
    }

    public Optional<TableOrViewMetadata> getTableOrView(String name) {
        return this.getObject(name, DefaultRDBSchemaMetadata::getTableOrView);
    }

}
