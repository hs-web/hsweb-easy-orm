package org.hswebframework.ezorm.rdb.supports.json;

import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlDriverUtils;
import org.postgresql.util.PGobject;

/**
 * Reads JSON values returned by PostgreSQL JDBC.
 */
public class JdbcPostgresqlJsonStringReader implements JsonStringReader {

    @Override
    public boolean supports(Object data) {
        return PostgresqlDriverUtils.isPgObject(data) && data instanceof PGobject;
    }

    @Override
    public String read(Object data) {
        if (PostgresqlDriverUtils.isPgObject(data) && data instanceof PGobject) {
            return ((PGobject) data).getValue();
        }
        return null;
    }
}
