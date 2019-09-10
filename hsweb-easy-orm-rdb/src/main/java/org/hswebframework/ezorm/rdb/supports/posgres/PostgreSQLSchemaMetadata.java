package org.hswebframework.ezorm.rdb.supports.posgres;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;

public class PostgreSQLSchemaMetadata extends RDBSchemaMetadata {

    public PostgreSQLSchemaMetadata(String name){
        super(name);
        addFeature(new PostgreSQLPaginator());
        addFeature(new PostgreSQLAlterTableSqlBuilder());
    }

}
