package org.hswebframework.ezorm.rdb.metadata.dialect;

import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.CustomDataType;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.JDBCType;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.*;

@Slf4j
public abstract class DefaultDialect implements Dialect {
    protected Map<String, DataTypeBuilder> dataTypeMappers = new HashMap<>();

    protected DataTypeBuilder defaultDataTypeBuilder;

    protected Map<String, JDBCType> jdbcTypeMap = new HashMap<>();

    protected Map<String, DataType> dataTypeMapping = new HashMap<>();

    protected Map<Class, JDBCType> classJDBCTypeMapping = new HashMap<>();

    protected void registerDataType(String symbol, DataType dataType) {
        dataTypeMapping.put(symbol, dataType);
    }

    public DefaultDialect() {
        defaultDataTypeBuilder = (meta) -> meta.getType().getName().toLowerCase();

        registerDataType("varchar", JdbcDataType.of(JDBCType.VARCHAR, String.class));
        registerDataType("bigint", JdbcDataType.of(JDBCType.BIGINT, Long.class));
        registerDataType("tinyint", JdbcDataType.of(JDBCType.TINYINT, Byte.class));
        registerDataType("timestamp", JdbcDataType.of(JDBCType.TIMESTAMP, Timestamp.class));
        registerDataType("date", JdbcDataType.of(JDBCType.DATE, LocalDate.class));
        registerDataType("time", JdbcDataType.of(JDBCType.TIME, LocalTime.class));
        registerDataType("clob", JdbcDataType.of(JDBCType.CLOB, String.class));
        registerDataType("blob", JdbcDataType.of(JDBCType.BLOB, byte[].class));
        registerDataType("numeric", JdbcDataType.of(JDBCType.NUMERIC, BigDecimal.class));
        registerDataType("number", JdbcDataType.of(JDBCType.NUMERIC, BigDecimal.class));
        registerDataType("long", JdbcDataType.of(JDBCType.BIGINT, Long.class));
        registerDataType("double", JdbcDataType.of(JDBCType.DOUBLE, Double.class));
        registerDataType("decimal", JdbcDataType.of(JDBCType.DECIMAL, BigDecimal.class));

        classJDBCTypeMapping.put(String.class, JDBCType.VARCHAR);

        classJDBCTypeMapping.put(Byte.class, JDBCType.TINYINT);
        classJDBCTypeMapping.put(byte.class, JDBCType.TINYINT);

        classJDBCTypeMapping.put(Short.class, JDBCType.SMALLINT);
        classJDBCTypeMapping.put(short.class, JDBCType.SMALLINT);

        classJDBCTypeMapping.put(Integer.class, JDBCType.INTEGER);
        classJDBCTypeMapping.put(int.class, JDBCType.INTEGER);

        classJDBCTypeMapping.put(Character.class, JDBCType.CHAR);
        classJDBCTypeMapping.put(char.class, JDBCType.CHAR);

        classJDBCTypeMapping.put(Long.class, JDBCType.BIGINT);
        classJDBCTypeMapping.put(long.class, JDBCType.BIGINT);

        classJDBCTypeMapping.put(Double.class, JDBCType.DOUBLE);
        classJDBCTypeMapping.put(double.class, JDBCType.DOUBLE);

        classJDBCTypeMapping.put(Float.class, JDBCType.FLOAT);
        classJDBCTypeMapping.put(float.class, JDBCType.FLOAT);

        classJDBCTypeMapping.put(Boolean.class, JDBCType.BOOLEAN);
        classJDBCTypeMapping.put(boolean.class, JDBCType.BOOLEAN);

        classJDBCTypeMapping.put(byte[].class, JDBCType.BLOB);

        classJDBCTypeMapping.put(BigDecimal.class, JDBCType.DECIMAL);
        classJDBCTypeMapping.put(BigInteger.class, JDBCType.INTEGER);

        classJDBCTypeMapping.put(Date.class, JDBCType.TIMESTAMP);
        classJDBCTypeMapping.put(java.sql.Date.class, JDBCType.TIMESTAMP);
        classJDBCTypeMapping.put(java.sql.Timestamp.class, JDBCType.TIMESTAMP);
        classJDBCTypeMapping.put(LocalTime.class, JDBCType.TIME);
        classJDBCTypeMapping.put(LocalDateTime.class, JDBCType.TIMESTAMP);
        classJDBCTypeMapping.put(LocalDate.class, JDBCType.DATE);

        classJDBCTypeMapping.put(Object.class, JDBCType.OTHER);

    }

    @Override
    public void addDataTypeMapper(String typeId, DataTypeBuilder mapper) {
        dataTypeMappers.put(typeId, mapper);
    }

    @Override
    public Optional<JDBCType> convertJdbcType(Class<?> type) {
        return Optional
                .ofNullable(classJDBCTypeMapping.get(type))
                .map(JDBCType.class::cast);
    }

    public void addJdbcTypeMapping(String dataType, JDBCType jdbcType) {
        jdbcTypeMap.put(dataType, jdbcType);
    }

    @Override
    public String createColumnDataType(RDBColumnMetadata columnMetaData) {
        if (columnMetaData.getType() == null) {
            return null;
        }
        DataType dataType = columnMetaData.getType();
        if (dataType instanceof DataTypeBuilder) {
            return ((DataTypeBuilder) dataType).createColumnDataType(columnMetaData);
        }
        DataTypeBuilder mapper = dataTypeMappers.get(columnMetaData.getType().getId());
        if (null == mapper) {
            mapper = defaultDataTypeBuilder;
        }
        return mapper.createColumnDataType(columnMetaData);
    }

    @Override
    public DataType convertDataType(String dataType) {

        return dataTypeMapping.getOrDefault(dataType, convertUnknownDataType(dataType));
    }

    protected DataType convertUnknownDataType(String dataType) {
        return CustomDataType.of(dataType, dataType, JDBCType.OTHER);
    }

    @Override
    public Optional<JDBCType> convertJdbcType(String dataType) {
        JDBCType jdbcType;
        try {
            jdbcType = JDBCType.valueOf(dataType.toUpperCase());
        } catch (Exception e) {
            if (dataType.contains("(")) {
                dataType = dataType.substring(0, dataType.indexOf("("));
            }
            jdbcType = jdbcTypeMap.get(dataType.toLowerCase());
            if (jdbcType == null) {
                //出现此警告可以通过 setJdbcTypeMapping注册一些奇怪的类型
                log.warn("can not parse jdbcType:{}", dataType);
                return Optional.empty();
            }
        }
        return Optional.of(jdbcType);
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
