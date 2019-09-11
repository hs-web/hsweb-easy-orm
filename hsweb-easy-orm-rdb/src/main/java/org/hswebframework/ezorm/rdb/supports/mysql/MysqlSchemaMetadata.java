package org.hswebframework.ezorm.rdb.supports.mysql;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;

public class MysqlSchemaMetadata extends RDBSchemaMetadata {

    public MysqlSchemaMetadata(String name) {
        super(name);

        addFeature(new MysqlCreateTableSqlBuilder());
        addFeature(new MysqlAlterTableSqlBuilder());
        addFeature(new MysqlPaginator());
        addFeature(MysqlIndexMetadataParser.of(this));
    }
}
