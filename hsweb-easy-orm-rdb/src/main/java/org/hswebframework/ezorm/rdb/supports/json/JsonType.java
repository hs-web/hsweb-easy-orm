package org.hswebframework.ezorm.rdb.supports.json;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.DataTypeBuilder;

import java.sql.JDBCType;
import java.sql.SQLType;

/**
 * JSON 列类型及其默认值编解码器，可由方言覆盖非空值的 SQL 表达式。
 * 默认沿用空参数绑定，原生 JSON 方言可覆盖其 SQL NULL 编码。
 * 解码结果保持 JSON 文本，结构化对象转换由显式 JSON codec 负责。
 */
@Getter
@AllArgsConstructor
public class JsonType implements JsonDataType, DataTypeBuilder, ValueCodec<Object, Object> {

    public static final JsonType INSTANCE = new JsonType("json", "json", "json", JDBCType.OTHER);

    public static final JsonType CLOB = new JsonType("json", "json", "clob", JDBCType.CLOB);

    public static final JsonType NVARCHAR_MAX = new JsonType("json", "json", "nvarchar(max)", JDBCType.LONGNVARCHAR);

    private final String id;

    private final String name;

    private final String columnDataType;

    private final SQLType sqlType;

    @Override
    public Class<?> getJavaType() {
        return String.class;
    }

    @Override
    public String createColumnDataType(RDBColumnMetadata columnMetaData) {
        return columnDataType;
    }

    @Override
    public Object encode(Object value) {
        return JsonCodecSupport.toJsonSilently(value);
    }

    @Override
    public Object decode(Object data) {
        return JsonCodecSupport.readAsString(data);
    }
}
