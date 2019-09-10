package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;

public class SqlServerSchemaMetadata extends RDBSchemaMetadata {

    public SqlServerSchemaMetadata() {
        super();

        addFeature(new SqlServerCreateTableSqlBuilder());
        addFeature(new SqlServerAlterTableSqlBuilder());
    }
}
