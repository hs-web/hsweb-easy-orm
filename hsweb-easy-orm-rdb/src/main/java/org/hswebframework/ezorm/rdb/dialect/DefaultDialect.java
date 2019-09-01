package org.hswebframework.ezorm.rdb.dialect;

import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.rdb.dialect.function.SqlFunction;
import org.hswebframework.ezorm.rdb.meta.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.TermFragmentBuilder;

import java.sql.JDBCType;
import java.util.*;

@Slf4j
public abstract class DefaultDialect implements Dialect {
    protected Map<String, DataTypeMapper> dataTypeMappers = new HashMap<>();
    protected DataTypeMapper defaultDataTypeMapper = null;

    protected Map<String, JDBCType> jdbcTypeMap = new HashMap<>();

    @Override
    public void setDataTypeMapper(JDBCType jdbcType, DataTypeMapper mapper) {
        dataTypeMappers.put(jdbcType.getName(), mapper);
    }

    public void setJdbcTypeMapping(String dataType, JDBCType jdbcType) {
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

    @Setter
    @Getter
    private boolean preparePagingSql = Boolean.getBoolean("easyorm.paging.prepare");

    @Override
    public String doPaging(String sql, int pageIndex, int pageSize) {
        return doPaging(sql, pageIndex, pageSize, preparePagingSql);
    }
}
