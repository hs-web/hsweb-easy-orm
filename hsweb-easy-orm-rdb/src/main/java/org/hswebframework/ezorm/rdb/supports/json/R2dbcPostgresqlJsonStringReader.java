package org.hswebframework.ezorm.rdb.supports.json;

import io.r2dbc.postgresql.codec.Json;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlDriverUtils;

/**
 * Reads JSON values returned by PostgreSQL R2DBC.
 */
public class R2dbcPostgresqlJsonStringReader implements JsonStringReader {

    @Override
    public boolean supports(Object data) {
        return PostgresqlDriverUtils.isR2dbcJson(data) && data instanceof Json;
    }

    @Override
    public String read(Object data) {
        if (PostgresqlDriverUtils.isR2dbcJson(data) && data instanceof Json) {
            return ((Json) data).asString();
        }
        return null;
    }
}
