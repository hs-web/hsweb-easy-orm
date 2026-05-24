package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.supports.json.JsonCodecSupport;

import java.sql.JDBCType;

public class JsonbType extends org.hswebframework.ezorm.rdb.supports.json.JsonType {

    public static JsonbType INSTANCE = new JsonbType();

    public JsonbType() {
        super("jsonb", "jsonb", "jsonb", JDBCType.OTHER);
    }

    @Override
    public Object encode(Object value) {
        String json = JsonCodecSupport.toJsonSilently(value);
        return json == null ? null : NativeSql.of("?::jsonb", json);
    }
}
