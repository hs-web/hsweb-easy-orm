package org.hswebframework.ezorm.rdb.metadata.dialect;

import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.rdb.metadata.*;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.JDBCType;
import java.sql.SQLType;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.*;

@Slf4j
public abstract class DefaultDialect implements Dialect {
    protected Map<String, DataTypeBuilder> dataTypeMappers = new HashMap<>();

    protected DataTypeBuilder defaultDataTypeBuilder;

    protected Map<String, DataType> dataTypeMapping = new HashMap<>();

    protected Map<Class<?>, JDBCType> classJDBCTypeMapping = new HashMap<>();

    protected void registerDataType(String symbol, DataType dataType) {
        dataTypeMapping.put(symbol, dataType instanceof DataTypeBuilder ? dataType : DataType.builder(dataType, meta -> symbol));
    }

    public DefaultDialect() {
        defaultDataTypeBuilder = (meta) -> meta.getType().getName().toLowerCase();

        registerDataType("varchar", DataType.builder(DataType.jdbc(JDBCType.VARCHAR, String.class),
                                                     column -> "varchar(" + column.getLength(255) + ")"));

        registerDataType("nvarchar", DataType.builder(DataType.jdbc(JDBCType.NVARCHAR, String.class),
                                                      column -> "nvarchar(" + column.getLength(255) + ")"));

        registerDataType("decimal", DataType.builder(DataType.jdbc(JDBCType.DECIMAL, BigDecimal.class),
                                                     column -> "decimal(" + column.getPrecision(32) + "," + column.getScale() + ")"));

        registerDataType("numeric", DataType.builder(DataType.jdbc(JDBCType.NUMERIC, BigDecimal.class),
                                                     column -> "numeric(" + column.getPrecision(32) + "," + column.getScale() + ")"));

        registerDataType("number", DataType.builder(DataType.jdbc(JDBCType.NUMERIC, BigDecimal.class),
                                                    column -> "number(" + column.getPrecision(32) + "," + column.getScale() + ")"));


        registerDataType("bigint", JdbcDataType.of(JDBCType.BIGINT, Long.class));
        registerDataType("tinyint", JdbcDataType.of(JDBCType.TINYINT, Byte.class));
        registerDataType("timestamp", JdbcDataType.of(JDBCType.TIMESTAMP, Timestamp.class));
        registerDataType("date", JdbcDataType.of(JDBCType.DATE, LocalDate.class));
        registerDataType("time", JdbcDataType.of(JDBCType.TIME, LocalTime.class));
        registerDataType("long", JdbcDataType.of(JDBCType.BIGINT, Long.class));
        registerDataType("double", JdbcDataType.of(JDBCType.DOUBLE, Double.class));
        registerDataType("binary", JdbcDataType.of(JDBCType.BINARY, byte[].class));

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

    protected void addDataTypeBuilder(JDBCType jdbcType, DataTypeBuilder mapper) {
        addDataTypeBuilder(jdbcType.getName().toLowerCase(), mapper);
    }

    @Override
    public void addDataTypeBuilder(String typeId, DataTypeBuilder mapper) {
        dataTypeMappers.put(typeId.toLowerCase(), mapper);
    }

    @Override
    public Optional<SQLType> convertSqlType(Class<?> type) {
        return Optional
            .ofNullable(classJDBCTypeMapping.get(type))
            .map(JDBCType.class::cast);
    }

    @Override
    public String buildColumnDataType(RDBColumnMetadata columnMetaData) {
        if (columnMetaData.getType() == null) {

            throw new UnsupportedOperationException("unknown column type : " + columnMetaData);
        }
        DataType dataType = columnMetaData.getType();
        if (dataType instanceof DataTypeBuilder) {
            return ((DataTypeBuilder) dataType).createColumnDataType(columnMetaData);
        }
        DataTypeBuilder mapper = dataTypeMappers.get(dataType.getId().toLowerCase());
        if (null == mapper) {
            mapper = defaultDataTypeBuilder;
        }
        return mapper.createColumnDataType(columnMetaData);
    }

    @Override
    public DataType convertDataType(String dataType) {
        String type = dataType;
        //length
        if (type.contains("(")) {
            type = type.substring(0, type.indexOf("("));
            String[] arr = dataType
                .substring(dataType.indexOf("(") + 1, dataType.lastIndexOf(")"))
                .split(",");
            int length = Integer.parseInt(arr[0].trim());
            int scale = arr.length > 1 ? Integer.parseInt(arr[1].trim()) : 0;
            return convertDataType(type, length, scale);
        }
        return dataTypeMapping.getOrDefault(type.toLowerCase(), convertUnknownDataType(dataType));
    }

    protected DataType convertDataType(String type, int length, int scale) {
        DataType staticType = dataTypeMapping.get(type);
        if (staticType == null) {
            return convertUnknownDataType(type);
        } else {
            return new LengthSupportDataType(
                staticType,
                length,
                length,
                scale
            );
        }
    }

    protected DataType convertUnknownDataType(String dataType) {
        JDBCType type;
        try {
            type = JDBCType.valueOf(dataType.toUpperCase());
        } catch (Exception e) {
            type = JDBCType.OTHER;
        }
        return CustomDataType.of(dataType, dataType, type, String.class);
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
