package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.supports.json.JsonCodecSupport;

import java.sql.JDBCType;

/** PostgreSQL 原生 JSON 类型，非空值使用类型转换表达式，空值使用 SQL NULL。 */
public class JsonType extends org.hswebframework.ezorm.rdb.supports.json.JsonType {

    public static JsonType INSTANCE = new JsonType();

    public JsonType() {
        super("json", "json", "json", JDBCType.OTHER);
    }

    @Override
    public Object encodeNull() {
        // PostgreSQL 不接受以 VARCHAR 类型绑定的 JSON SQL NULL。
        return NativeSql.of("null");
    }

    @Override
    public Object encode(Object value) {
        String json = JsonCodecSupport.toJsonSilently(value);
        return json == null ? null : NativeSql.of("?::json", json);
    }
}
