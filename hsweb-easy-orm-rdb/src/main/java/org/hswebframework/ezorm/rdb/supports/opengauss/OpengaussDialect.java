package org.hswebframework.ezorm.rdb.supports.opengauss;

import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ValuesSqlFragments;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlDialect;

import java.util.List;

/**
 * @author zhouhao
 * @since 3.0
 */
public class OpengaussDialect extends PostgresqlDialect {

    public static final Dialect global = new OpengaussDialect();

    public OpengaussDialect() {
        super();
    }

    @Override
    public SqlFragments buildValues(List<RDBColumnMetadata> columns, List<Object[]> rows) {
        // 保持普通值集语法，不继承 PostgreSQL 专用的多数组 UNNEST 与驱动绑定。
        return ValuesSqlFragments.of(columns, rows);
    }

    @Override
    public String getId() {
        return "opengauss";
    }

    @Override
    public String getName() {
        return "Opengauss";
    }
}
