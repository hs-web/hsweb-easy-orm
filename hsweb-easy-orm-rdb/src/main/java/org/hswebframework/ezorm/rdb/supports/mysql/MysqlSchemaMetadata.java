package org.hswebframework.ezorm.rdb.supports.mysql;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
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

    @Override
    public RDBTableMetadata newTable(String name) {
        RDBTableMetadata metadata= super.newTable(name);
        metadata.addFeature(new MysqlSaveOrUpdateOperator(metadata));
        return metadata;
    }

    @Override
    public void addTable(RDBTableMetadata metadata) {
        metadata.addFeature(new MysqlSaveOrUpdateOperator(metadata));
        super.addTable(metadata);
    }
}
