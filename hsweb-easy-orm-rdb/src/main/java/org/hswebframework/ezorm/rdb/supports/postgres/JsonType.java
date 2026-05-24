package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.supports.json.JsonCodecSupport;

import java.sql.JDBCType;

public class JsonType extends org.hswebframework.ezorm.rdb.supports.json.JsonType {

    public static JsonType INSTANCE = new JsonType();

    public JsonType() {
        super("json", "json", "json", JDBCType.OTHER);
    }

    @Override
    public Object encode(Object value) {
        String json = JsonCodecSupport.toJsonSilently(value);
        return json == null ? null : NativeSql.of("?::json", json);
    }
}
