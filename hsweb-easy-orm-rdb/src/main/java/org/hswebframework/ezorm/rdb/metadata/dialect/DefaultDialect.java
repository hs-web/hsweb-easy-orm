package org.hswebframework.ezorm.rdb.metadata.dialect;

import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;

import java.sql.JDBCType;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

@Slf4j
public abstract class DefaultDialect implements Dialect {
    protected Map<String, DataTypeMapper> dataTypeMappers = new HashMap<>();

    protected DataTypeMapper defaultDataTypeMapper = null;

    protected Map<String, JDBCType> jdbcTypeMap = new HashMap<>();

    @Override
    public void addDataTypeMapper(JDBCType jdbcType, DataTypeMapper mapper) {
        dataTypeMappers.put(jdbcType.getName(), mapper);
    }

    public void addJdbcTypeMapping(String dataType, JDBCType jdbcType) {
        jdbcTypeMap.put(dataType, jdbcType);
    }

    @Override
    public String buildDataType(RDBColumnMetadata columnMetaData) {
        if (columnMetaData.getJdbcType() == null) {
            return null;
        }
        DataTypeMapper mapper = dataTypeMappers.get(columnMetaData.getJdbcType().getName());
        if (null == mapper) {
            mapper = defaultDataTypeMapper;
        }
        return mapper.getDataType(columnMetaData);
    }

    @Override
    public JDBCType getJdbcType(String dataType) {
        JDBCType jdbcType;
        try {
            jdbcType = JDBCType.valueOf(dataType.toUpperCase());
        } catch (Exception e) {
            if (dataType.contains("("))
                dataType = dataType.substring(0, dataType.indexOf("("));
            jdbcType = jdbcTypeMap.get(dataType.toLowerCase());
            if (jdbcType == null) {
                //出现此警告可以通过 setJdbcTypeMapping注册一些奇怪的类型
                log.warn("can not parse jdbcType:{}", dataType);
                jdbcType = JDBCType.OTHER;
            }
        }
        return jdbcType;
    }


    protected String doClearQuote(String string) {
        if (string.startsWith(getQuoteStart())) {
            string = string.substring(getQuoteStart().length());
        }
        if (string.endsWith(getQuoteEnd())) {
            string = string.substring(0, string.length() - getQuoteEnd().length());
        }
        return string;
    }

    @Override
    public String clearQuote(String string) {
        if (string == null || string.isEmpty()) {
            return string;
        }
        if (string.contains(".")) {
            String[] arr = string.split("[.]");
            for (int i = 0; i < arr.length; i++) {
                arr[i] = doClearQuote(arr[i]);
            }
            return String.join(".", arr);
        }

        return doClearQuote(string);
    }

}
