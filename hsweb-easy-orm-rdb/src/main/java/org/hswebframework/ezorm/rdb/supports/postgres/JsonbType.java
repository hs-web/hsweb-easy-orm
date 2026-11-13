package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.supports.json.JsonCodecSupport;

import java.sql.JDBCType;

/** PostgreSQL 原生 JSONB 类型，非空值使用类型转换表达式，空值使用 SQL NULL。 */
public class JsonbType extends org.hswebframework.ezorm.rdb.supports.json.JsonType {

    public static JsonbType INSTANCE = new JsonbType();

    public JsonbType() {
        super("jsonb", "jsonb", "jsonb", JDBCType.OTHER);
    }

    @Override
    public Object encodeNull() {
        // PostgreSQL 不接受以 VARCHAR 类型绑定的 JSONB SQL NULL。
        return NativeSql.of("null");
    }

    @Override
    public Object encode(Object value) {
        String json = JsonCodecSupport.toJsonSilently(value);
        return json == null ? null : NativeSql.of("?::jsonb", json);
    }
}
