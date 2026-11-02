package org.hswebframework.ezorm.rdb.supports.opengauss;

import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlDialect;

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
    public String getId() {
        return "opengauss";
    }

    @Override
    public String getName() {
        return "Opengauss";
    }
}
