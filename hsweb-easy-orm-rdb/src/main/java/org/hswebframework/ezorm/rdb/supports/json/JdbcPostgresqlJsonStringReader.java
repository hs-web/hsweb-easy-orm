package org.hswebframework.ezorm.rdb.supports.json;

import org.postgresql.util.PGobject;

/**
 * Reads JSON values returned by PostgreSQL JDBC.
 */
public class JdbcPostgresqlJsonStringReader implements JsonStringReader {

    @Override
    public boolean supports(Object data) {
        return data instanceof PGobject;
    }

    @Override
    public String read(Object data) {
        String value = ((PGobject) data).getValue();
        return value == null ? null : String.valueOf(value);
    }
}
