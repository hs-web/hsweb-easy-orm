package org.hswebframework.ezorm.rdb.supports.kingbase.oracle;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateIndexParameter;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateIndexSqlBuilder;

import static org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments.of;

/**
 * Renders the PostgreSQL-compatible index DDL accepted by KingbaseES Oracle
 * compatibility mode.
 */
public class KingbaseOracleCreateIndexSqlBuilder implements CreateIndexSqlBuilder {

    public static final KingbaseOracleCreateIndexSqlBuilder INSTANCE = new KingbaseOracleCreateIndexSqlBuilder();

    @Override
    public SqlRequest build(CreateIndexParameter parameter) {
        RDBIndexMetadata index = parameter.getIndex();
        RDBTableMetadata table = parameter.getTable();
        PrepareSqlFragments fragments = of()
            .addSql("create ", index.isUnique() ? "unique" : "", " index ",
                    index.getName(), " on ", table.getFullName(), "(");

        int position = 0;
        for (RDBIndexMetadata.IndexColumn column : index.getColumns()) {
            RDBColumnMetadata columnMetadata = table.getColumn(column.getColumn())
                .orElseThrow(() -> new UnsupportedOperationException(
                    "Undefined index column: " + table.getName() + "." + column.getColumn()));

            if (position++ != 0) {
                fragments.addSql(",");
            }
            fragments.addSql(table.getDialect().quote(columnMetadata.getName()))
                     .addSql(column.getSort().name());
        }
        return fragments.addSql(")").toRequest();
    }
}
