package org.hswebframework.ezorm.rdb.supports.h2;

import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;

public class H2DatabaseMetadata extends RDBDatabaseMetadata {

    public H2DatabaseMetadata() {
        super(Dialect.H2);
        addFeature(new H2CreateTableSqlBuilder());

    }
}
