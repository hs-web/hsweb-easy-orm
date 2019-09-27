package org.hswebframework.ezorm.rdb.metadata;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.sql.JDBCType;
import java.sql.SQLType;

@Getter
@AllArgsConstructor(staticName = "of")
public class CustomDataType implements DataType {

    private String id;

    private String name;

    private SQLType sqlType;

    private Class javaType;

}
