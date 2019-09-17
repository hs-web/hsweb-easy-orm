package org.hswebframework.ezorm.rdb.supports.mysql;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;

public class MysqlSchemaMetadata extends RDBSchemaMetadata {

    public MysqlSchemaMetadata(String name) {
        super(name);

        addFeature(new MysqlCreateTableSqlBuilder());
        addFeature(new MysqlAlterTableSqlBuilder());
        addFeature(new MysqlPaginator());

        addFeature(new MysqlIndexMetadataParser(this));
        addFeature(new MysqlTableMetadataParser(this));
        addFeature(Dialect.MYSQL);
    }
}
