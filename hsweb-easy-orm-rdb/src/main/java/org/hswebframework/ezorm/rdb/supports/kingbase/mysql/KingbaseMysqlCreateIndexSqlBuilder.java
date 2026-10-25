package org.hswebframework.ezorm.rdb.supports.kingbase.mysql;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateIndexParameter;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateIndexSqlBuilder;

import static org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments.of;

/**
 * Renders MySQL-compatible index DDL for KingbaseES.
 */
public class KingbaseMysqlCreateIndexSqlBuilder implements CreateIndexSqlBuilder {

    public static final KingbaseMysqlCreateIndexSqlBuilder INSTANCE = new KingbaseMysqlCreateIndexSqlBuilder();

    @Override
    public SqlRequest build(CreateIndexParameter parameter) {
        RDBIndexMetadata index = parameter.getIndex();
        RDBTableMetadata table = parameter.getTable();
        PrepareSqlFragments fragments = of()
            .addSql("create ", index.isUnique() ? "unique" : "", " index ",
                    index.getName(), " on ", table.getFullName(), "(");
        int i = 0;
        for (RDBIndexMetadata.IndexColumn column : index.getColumns()) {
            RDBColumnMetadata columnMetadata = table
                .getColumn(column.getColumn())
                .orElseThrow(() -> new UnsupportedOperationException("未定义的索引列:" + table.getName() + "." + column.getColumn()));

            if (i++ != 0) {
                fragments.addSql(",");
            }
            fragments.addSql(table.getDialect().quote(columnMetadata.getName()))
                     .addSql(column.getSort().name());
        }
        return fragments.addSql(")").toRequest();
    }
}
