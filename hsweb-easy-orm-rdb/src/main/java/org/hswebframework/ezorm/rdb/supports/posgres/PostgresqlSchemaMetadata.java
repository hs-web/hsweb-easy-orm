package org.hswebframework.ezorm.rdb.supports.posgres;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;

public class PostgresqlSchemaMetadata extends RDBSchemaMetadata {

    public PostgresqlSchemaMetadata(String name) {
        super(name);
        addFeature(new PostgresqlPaginator());
        addFeature(new PostgresqlAlterTableSqlBuilder());

        addFeature(new PostgresqlTableMetadataParser(this));

        addFeature(Dialect.POSTGRES);
    }

}
