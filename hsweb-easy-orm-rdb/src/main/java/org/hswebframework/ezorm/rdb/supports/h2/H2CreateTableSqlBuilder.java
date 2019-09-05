package org.hswebframework.ezorm.rdb.supports.h2;

import org.hswebframework.ezorm.core.DefaultValue;
import org.hswebframework.ezorm.rdb.executor.DefaultBatchSqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateTableSqlBuilder;

import static org.hswebframework.ezorm.rdb.executor.SqlRequests.*;

public class H2CreateTableSqlBuilder implements CreateTableSqlBuilder {

    @Override
    public SqlRequest build(RDBTableMetadata table) {
        DefaultBatchSqlRequest sql = new DefaultBatchSqlRequest();

        PrepareSqlFragments createTable = PrepareSqlFragments.of();

        createTable.addSql("create table", table.getFullName(), "(");

        int index = 0;
        for (RDBColumnMetadata column : table.getColumns()) {
            if (index++ != 0) {
                createTable.addSql(",");
            }
            createTable.addSql(column.getDialect().quote(getName()));
            if (column.getColumnDefinition() != null) {
                createTable.addSql(column.getColumnDefinition());
            } else {
                createTable.addSql(column.getDialect().buildDataType(column));
                if (column.isNotNull() || column.isPrimaryKey()) {
                    createTable.addSql("not null");
                }
                if (column.isPrimaryKey()) {
                    createTable.addSql("primary key");
                }
                DefaultValue defaultValue = column.getDefaultValue();
                if (defaultValue instanceof NativeSql) {
                    createTable.addSql("default", ((NativeSql) defaultValue).getSql());
                }
            }
            if (column.getComment() != null) {
                sql.addBatch(of(String.format("comment on column %s is '%s'", column.getFullName(), column.getComment())));
            }
        }
        createTable.addSql(")");

        if (table.getComment() != null) {
            sql.addBatch(of(String.format("comment on table %s is '%s'", table.getFullName(), table.getComment())));
        }

        sql.setSql(createTable.toRequest().getSql());

        return sql;
    }

}
