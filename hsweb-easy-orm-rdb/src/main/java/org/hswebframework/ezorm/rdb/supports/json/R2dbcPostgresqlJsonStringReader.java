package org.hswebframework.ezorm.rdb.supports.json;

import io.r2dbc.postgresql.codec.Json;

/**
 * Reads JSON values returned by PostgreSQL R2DBC.
 */
public class R2dbcPostgresqlJsonStringReader implements JsonStringReader {

    @Override
    public boolean supports(Object data) {
        return data instanceof Json;
    }

    @Override
    public String read(Object data) {
        return ((Json) data).asString();
    }
}
