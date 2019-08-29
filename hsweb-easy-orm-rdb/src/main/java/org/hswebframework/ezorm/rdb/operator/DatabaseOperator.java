package org.hswebframework.ezorm.rdb.operator;

import org.hswebframework.ezorm.rdb.meta.DefaultRDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.meta.DefaultRDBSchemaMetadata;

public interface DatabaseOperator {

    DefaultRDBDatabaseMetadata<DefaultRDBSchemaMetadata> getMetadata();

    DMLOperator dml();

    DDLOperator ddl();

    SQLOperator sql();

}
