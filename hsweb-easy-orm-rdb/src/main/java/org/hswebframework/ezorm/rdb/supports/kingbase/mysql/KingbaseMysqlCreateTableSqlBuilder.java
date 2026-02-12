package org.hswebframework.ezorm.rdb.supports.kingbase.mysql;

import org.hswebframework.ezorm.core.DefaultValue;
import org.hswebframework.ezorm.core.utils.StringUtils;
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
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlCreateTableSqlBuilder;

/**
 * KingbaseES MySQL 兼容模式的建表 SQL 构建器.
 * <p>
 * 与 {@link MysqlCreateTableSqlBuilder} 的区别：
 * <ul>
 *   <li>去掉了 {@code ENGINE=InnoDB DEFAULT CHARSET=utf8mb4} 等 MySQL 特有子句</li>
 *   <li>保留了 {@code auto_increment}（KingbaseES MySQL 兼容版支持）</li>
 *   <li>保留了 {@code comment} 语法</li>
 * </ul>
 *
 * @since 4.2
 */
@SuppressWarnings("all")
public class KingbaseMysqlCreateTableSqlBuilder implements CreateTableSqlBuilder {

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
            createTable.addSql(column.getQuoteName());
            if (column.getColumnDefinition() != null) {
                createTable.addSql(column.getColumnDefinition());
            } else {
                createTable.addSql(column.getDialect().buildColumnDataType(column));
                if (column.isNotNull() || column.isPrimaryKey()) {
                    createTable.addSql("not null");
                }
                if (column.isPrimaryKey()) {
                    createTable.addSql("primary key");
                }
                if (column.isAutoIncrement()) {
                    createTable.addSql("auto_increment");
                } else {
                    DefaultValue defaultValue = column.getDefaultValue();
                    if (defaultValue instanceof NativeSql) {
                        createTable.addSql("default", ((NativeSql) defaultValue).getSql());
                    }
                }
                if (column.getComment() != null) {
                    createTable.addSql(" comment ",
                                       StringUtils.concat("'", column.getComment(), "'"));
                }
            }
        }

        // KingbaseES 不支持 ENGINE= 和 DEFAULT CHARSET=，直接关闭括号
        createTable.addSql(")");

        if (table.getComment() != null) {
            createTable.addSql("COMMENT=", StringUtils.concat("'", table.getComment(), "'"));
        }

        sql.setSql(createTable.toRequest().getSql());

        table.findFeature(CreateIndexSqlBuilder.ID)
                .ifPresent(builder -> {
                    for (RDBIndexMetadata tableIndex : table.getIndexes()) {
                        sql.addBatch(builder.build(CreateIndexParameter.of(table, tableIndex)));
                    }
                });

        return sql;
    }
}
