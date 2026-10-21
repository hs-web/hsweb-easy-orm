package org.hswebframework.ezorm.rdb.supports.kingbase.oracle;

import org.hswebframework.ezorm.core.DefaultValue;
import org.hswebframework.ezorm.rdb.executor.DefaultBatchSqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateIndexParameter;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateIndexSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateTableSqlBuilder;

import java.util.ArrayList;
import java.util.List;

import static org.hswebframework.ezorm.rdb.executor.SqlRequests.of;

/**
 * Creates tables using Oracle-compatible types and KingbaseES comment/index
 * statements.
 */
public class KingbaseOracleCreateTableSqlBuilder implements CreateTableSqlBuilder {

    @Override
    public SqlRequest build(RDBTableMetadata table) {
        DefaultBatchSqlRequest result = new DefaultBatchSqlRequest();
        List<SqlRequest> comments = new ArrayList<>();
        PrepareSqlFragments createTable = PrepareSqlFragments.of();
        createTable.addSql("create table", table.getFullName(), "(");

        int position = 0;
        for (RDBColumnMetadata column : table.getColumns()) {
            if (position++ != 0) {
                createTable.addSql(",");
            }
            createTable.addSql(column.getQuoteName());
            if (column.getColumnDefinition() != null) {
                createTable.addSql(column.getColumnDefinition());
            } else {
                createTable.addSql(column.getDialect().buildColumnDataType(column));
                DefaultValue defaultValue = column.getDefaultValue();
                if (defaultValue instanceof NativeSql) {
                    createTable.addSql("default", ((NativeSql) defaultValue).getSql());
                }
                if (column.isNotNull() || column.isPrimaryKey()) {
                    createTable.addSql("not null");
                }
                if (column.isPrimaryKey()) {
                    createTable.addSql("primary key");
                }
            }
            if (column.getComment() != null) {
                comments.add(of(String.format("comment on column %s is '%s'",
                    column.getFullTableName(), column.getComment().replace("'", "''"))));
            }
        }
        createTable.addSql(")");

        if (table.getComment() != null) {
            comments.add(of(String.format("comment on table %s is '%s'",
                table.getFullName(), table.getComment().replace("'", "''"))));
        }

        result.setSql(createTable.toRequest().getSql());
        comments.forEach(result::addBatch);
        table.findFeature(CreateIndexSqlBuilder.ID).ifPresent(builder -> {
            for (RDBIndexMetadata tableIndex : table.getIndexes()) {
                result.addBatch(builder.build(CreateIndexParameter.of(table, tableIndex)));
            }
        });
        return result;
    }
}
