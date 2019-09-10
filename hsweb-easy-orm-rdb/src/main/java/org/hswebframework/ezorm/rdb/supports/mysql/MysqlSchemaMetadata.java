package org.hswebframework.ezorm.rdb.supports.mysql;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;

public class MysqlSchemaMetadata extends RDBSchemaMetadata {

    public MysqlSchemaMetadata(){
        super();

        addFeature(new MysqlCreateTableSqlBuilder());
        addFeature(new MysqlAlterTableSqlBuilder());
    }
}
