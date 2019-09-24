package org.hswebframework.ezorm.rdb.metadata.dialect;

import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.JDBCType;
import java.util.*;

@Slf4j
public abstract class DefaultDialect implements Dialect {
    protected Map<String, DataTypeMapper> dataTypeMappers = new HashMap<>();

    protected DataTypeMapper defaultDataTypeMapper;

    protected Map<String, JDBCType> jdbcTypeMap = new HashMap<>();

    protected Map<Class, JDBCType> classJDBCTypeMapping = new HashMap<>();

    public DefaultDialect() {
        defaultDataTypeMapper = (meta) -> meta.getType().getName().toLowerCase();

        classJDBCTypeMapping.put(String.class, JDBCType.VARCHAR);

        classJDBCTypeMapping.put(Byte.class, JDBCType.TINYINT);
        classJDBCTypeMapping.put(byte.class, JDBCType.TINYINT);

        classJDBCTypeMapping.put(Short.class, JDBCType.INTEGER);
        classJDBCTypeMapping.put(short.class, JDBCType.INTEGER);

        classJDBCTypeMapping.put(Integer.class, JDBCType.INTEGER);
        classJDBCTypeMapping.put(int.class, JDBCType.INTEGER);

        classJDBCTypeMapping.put(Character.class, JDBCType.CHAR);
        classJDBCTypeMapping.put(char.class, JDBCType.CHAR);

        classJDBCTypeMapping.put(Long.class, JDBCType.BIGINT);
        classJDBCTypeMapping.put(long.class, JDBCType.BIGINT);

        classJDBCTypeMapping.put(Double.class, JDBCType.DECIMAL);
        classJDBCTypeMapping.put(double.class, JDBCType.DECIMAL);

        classJDBCTypeMapping.put(Float.class, JDBCType.DECIMAL);
        classJDBCTypeMapping.put(float.class, JDBCType.DECIMAL);

        classJDBCTypeMapping.put(Boolean.class, JDBCType.BIT);
        classJDBCTypeMapping.put(boolean.class, JDBCType.BIT);

        classJDBCTypeMapping.put(byte[].class, JDBCType.BLOB);

        classJDBCTypeMapping.put(BigDecimal.class, JDBCType.DECIMAL);
        classJDBCTypeMapping.put(BigInteger.class, JDBCType.INTEGER);

        classJDBCTypeMapping.put(Date.class, JDBCType.TIMESTAMP);
        classJDBCTypeMapping.put(java.sql.Date.class, JDBCType.TIMESTAMP);
        classJDBCTypeMapping.put(java.sql.Timestamp.class, JDBCType.TIMESTAMP);

        classJDBCTypeMapping.put(Object.class, JDBCType.VARCHAR);

    }

    @Override
    public void addDataTypeMapper(JDBCType jdbcType, DataTypeMapper mapper) {
        dataTypeMappers.put(jdbcType.getName().toLowerCase(), mapper);
    }

    @Override
    public Optional<JDBCType> getJdbcType(Class<?> type) {
        return Optional.ofNullable(classJDBCTypeMapping.get(type));
    }

    public void addJdbcTypeMapping(String dataType, JDBCType jdbcType) {
        jdbcTypeMap.put(dataType, jdbcType);
    }

    @Override
    public String createColumnDataType(RDBColumnMetadata columnMetaData) {
        if (columnMetaData.getType() == null) {
            return null;
        }
        DataTypeMapper mapper = dataTypeMappers.get(columnMetaData.getType().getId());
        if (null == mapper) {
            mapper = defaultDataTypeMapper;
        }
        return mapper.createColumnDataType(columnMetaData);
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
