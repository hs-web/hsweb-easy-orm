package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;

public class SqlServerSchemaMetadata extends RDBSchemaMetadata {

    public SqlServerSchemaMetadata(String name) {
        super(name);
        addFeature(new SqlServerCreateTableSqlBuilder());
        addFeature(new SqlServerAlterTableSqlBuilder());
        addFeature(new SqlServer2012Paginator());
    }
}
