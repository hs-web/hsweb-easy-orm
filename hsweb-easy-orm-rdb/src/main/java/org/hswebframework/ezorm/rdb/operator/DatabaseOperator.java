package org.hswebframework.ezorm.rdb.operator;

import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;

public interface DatabaseOperator {

    RDBDatabaseMetadata getMetadata();

    DMLOperator dml();

    DDLOperator ddl();

    SQLOperator sql();

}
