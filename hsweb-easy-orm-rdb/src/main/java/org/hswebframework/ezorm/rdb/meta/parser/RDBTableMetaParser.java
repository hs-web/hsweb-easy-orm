package org.hswebframework.ezorm.rdb.meta.parser;

import lombok.Getter;
import lombok.Setter;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.core.ObjectWrapper;
import org.hswebframework.ezorm.core.meta.ObjectMetaDataParserStrategy;
import org.hswebframework.ezorm.core.meta.ObjectType;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.meta.RDBColumnMetaData;
import org.hswebframework.ezorm.rdb.meta.RDBObjectType;
import org.hswebframework.ezorm.rdb.meta.RDBTableMetaData;
import org.hswebframework.ezorm.rdb.meta.expand.SimpleMapWrapper;
import org.hswebframework.ezorm.rdb.dialect.Dialect;
import org.hswebframework.utils.StringUtils;

import java.math.BigDecimal;
import java.sql.JDBCType;
import java.util.*;
import java.util.stream.Collectors;

import static org.hswebframework.ezorm.rdb.executor.SqlRequest.prepare;
import static org.hswebframework.ezorm.rdb.executor.SqlRequest.template;

public abstract class RDBTableMetaParser implements ObjectMetaDataParserStrategy<RDBTableMetaData> {

    private static final Map<JDBCType, Class> defaultJavaTypeMap = new HashMap<>();

    static {
        defaultJavaTypeMap.put(JDBCType.VARCHAR, String.class);
        defaultJavaTypeMap.put(JDBCType.CLOB, String.class);
        defaultJavaTypeMap.put(JDBCType.BLOB, byte[].class);
        defaultJavaTypeMap.put(JDBCType.DATE, Date.class);
        defaultJavaTypeMap.put(JDBCType.TIME, Date.class);
        defaultJavaTypeMap.put(JDBCType.TIMESTAMP, Date.class);
        defaultJavaTypeMap.put(JDBCType.BIT, Byte.class);
        defaultJavaTypeMap.put(JDBCType.BIGINT, Long.class);
        defaultJavaTypeMap.put(JDBCType.INTEGER, Integer.class);
        defaultJavaTypeMap.put(JDBCType.DOUBLE, Double.class);
        defaultJavaTypeMap.put(JDBCType.FLOAT, Float.class);
        defaultJavaTypeMap.put(JDBCType.CHAR, String.class);
        defaultJavaTypeMap.put(JDBCType.TINYINT, Byte.class);
        defaultJavaTypeMap.put(JDBCType.NUMERIC, BigDecimal.class);
        defaultJavaTypeMap.put(JDBCType.DECIMAL, BigDecimal.class);
    }

    @Getter
    @Setter
    private String schemaName;

    protected SyncSqlExecutor sqlExecutor;

    protected Map<JDBCType, Class> javaTypeMap = new HashMap<>();

    protected abstract Dialect getDialect();

    protected  abstract String getTableMetaSql(String name);

    protected  abstract String getTableCommentSql(String name);

    protected abstract String getAllTableSql();

    protected  abstract String getTableExistsSql();

    public RDBTableMetaParser(SyncSqlExecutor executor) {
        this.sqlExecutor = executor;
    }

    @Override
    public ObjectType getSupportType() {
        return RDBObjectType.table;
    }

    @SneakyThrows
    protected Optional<RDBTableMetaData> doParse(String name) {
        RDBTableMetaData metaData = new RDBTableMetaData();
        metaData.setName(name);
        metaData.setAlias(name);
        Map<String, Object> param = new HashMap<>();
        param.put("table", name);

        //列
        List<RDBColumnMetaData> metaDataList = sqlExecutor.select(template(getTableMetaSql(name), param), columnMetaDataWrapper);
        metaDataList.forEach(metaData::addColumn);
        //说明
        Map<String, Object> comment = sqlExecutor.selectSingle(template(getTableCommentSql(name), param), lowerCasePropertySimpleMapWrapper);
        if (null != comment && comment.get("comment") != null) {
            metaData.setComment(String.valueOf(comment.get("comment")));
        }

        return Optional.of(metaData);
    }

    @Override
    public Optional<RDBTableMetaData> parse(String name) {
        if (!objectExists(name)) {
            return Optional.empty();
        }
        return doParse(name);
    }

    @Override
    @SneakyThrows
    public boolean objectExists(String name) {
        Map<String, Object> param = new HashMap<>();
        param.put("table", name);
        Map<String, Object> res = sqlExecutor.selectSingle(template(getTableExistsSql(), param), lowerCasePropertySimpleMapWrapper);
        return res.get("total") != null && StringUtils.toInt(res.get("total")) > 0;
    }

    @Override
    @SneakyThrows
    public Set<String> getAllNames() {
        return sqlExecutor
                .select(prepare(getAllTableSql()), lowerCasePropertySimpleMapWrapper)
                .stream()
                .map(map -> map.get("name"))
                .filter(Objects::nonNull)
                .map(String::valueOf)
                .collect(Collectors.toSet());
    }

    @Override
    public List<RDBTableMetaData> parseAll() {
        return getAllNames()
                .parallelStream()
                .map(this::doParse)
                .filter(Optional::isPresent)
                .map(Optional::get)
                .collect(Collectors.toList());
    }

    protected static LowerCasePropertySimpleMapWrapper lowerCasePropertySimpleMapWrapper = new LowerCasePropertySimpleMapWrapper();

    protected RDBColumnMetaDataWrapper columnMetaDataWrapper = new RDBColumnMetaDataWrapper();

    static class LowerCasePropertySimpleMapWrapper extends SimpleMapWrapper {
        @Override
        public void wrapper(Map<String, Object> instance, int index, String attr, Object value) {
            attr = attr.toLowerCase();
            super.wrapper(instance, index, attr, value);
        }
    }

    @SuppressWarnings("all")
    class RDBColumnMetaDataWrapper implements ObjectWrapper<RDBColumnMetaData> {
        @Override
        public Class<RDBColumnMetaData> getType() {
            return RDBColumnMetaData.class;
        }

        @Override
        public RDBColumnMetaData newInstance() {
            return new RDBColumnMetaData();
        }

        @Override
        public void wrapper(RDBColumnMetaData instance, int index, String attr, Object value) {
            String stringValue;
            if (value instanceof String) {
                stringValue = ((String) value).toLowerCase();
            } else {
                stringValue = value == null ? "" : value.toString();
            }
            if (attr.equalsIgnoreCase("name")) {
                instance.setName(stringValue);
                instance.setProperty("old-name", stringValue);
            } else if (attr.equalsIgnoreCase("comment")) {
                instance.setComment(stringValue);
            } else {
                if (attr.toLowerCase().equals("not-null")) {
                    value = "1".equals(stringValue);
                    instance.setNotNull((boolean) value);
                }
                instance.setProperty(attr.toLowerCase(), value);
            }
        }

        @Override
        public boolean done(RDBColumnMetaData instance) {
            String data_type = instance.getProperty("data_type").toString().toLowerCase();
            int len = instance.getProperty("data_length").toInt();
            int data_precision = instance.getProperty("data_precision").toInt();
            int data_scale = instance.getProperty("data_scale").toInt();
            instance.setLength(len);
            instance.setPrecision(data_precision);
            instance.setScale(data_scale);

            JDBCType jdbcType = getDialect().getJdbcType(data_type);
            Class javaType = Optional.ofNullable(javaTypeMap.get(jdbcType))
                    .orElseGet(() -> defaultJavaTypeMap.getOrDefault(jdbcType, String.class));

            instance.setJdbcType(jdbcType);
            instance.setJavaType(javaType);
            instance.setDataType(getDialect().buildDataType(instance));
            return true;
        }
    }
}
