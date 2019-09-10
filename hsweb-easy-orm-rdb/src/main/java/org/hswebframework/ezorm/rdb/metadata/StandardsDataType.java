package org.hswebframework.ezorm.rdb.metadata;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum StandardsDataType implements DataType {

    CHAR("定长字符串"),
    VARCHAR("字符串"),

    NUMERIC("数字"),
    BOOLEAN("布尔值"),
    TINYINT("小整数值"),
    INTEGER("整数值"),
    BIGINT("大整数值"),
    TIMESTAMP("时间戳"),
    DATETIME("时间"),
    DATE("日期"),

    /**
     * bitand
     */
    BITMASK("位掩码"),
    CLOB("大文本"),
    BLOB("二进制数据"),
    JSON("JSON"),
    JSONB("二进制JSON");

    private String name;

    @Override
    public String getId() {
        return name();
    }


}
