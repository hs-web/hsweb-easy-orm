package org.hswebframework.ezorm.rdb.supports.commons;

import lombok.Getter;
import lombok.Setter;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.core.meta.ObjectMetaDataParserStrategy;
import org.hswebframework.ezorm.core.meta.ObjectType;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.*;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBObjectType;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.utils.StringUtils;

import java.math.BigDecimal;
import java.sql.JDBCType;
import java.util.*;
import java.util.stream.Collectors;

import static org.hswebframework.ezorm.rdb.executor.SqlRequests.template;
import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.list;

public abstract class RDBTableMetaParser implements ObjectMetaDataParserStrategy<RDBTableMetadata> {

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

    protected abstract String getTableMetaSql(String name);

    protected abstract String getTableCommentSql(String name);

    protected abstract String getAllTableSql();

    protected abstract String getTableExistsSql();

    public RDBTableMetaParser(SyncSqlExecutor executor) {
        this.sqlExecutor = executor;
    }

    @Override
    public ObjectType getSupportType() {
        return RDBObjectType.table;
    }

    @SneakyThrows
    protected Optional<RDBTableMetadata> doParse(String name) {
        RDBTableMetadata metaData = new RDBTableMetadata();
        metaData.setName(name);
        metaData.setAlias(name);
        Map<String, Object> param = new HashMap<>();
        param.put("table", name);

        //列
        List<RDBColumnMetadata> metaDataList = sqlExecutor.select(template(getTableMetaSql(name), param), list(columnMetaDataWrapper));
        metaDataList.forEach(metaData::addColumn);
        //说明
        Map<String, Object> comment = sqlExecutor.select(template(getTableCommentSql(name), param), ResultWrappers.singleMap());
        if (null != comment && comment.get("comment") != null) {
            metaData.setComment(String.valueOf(comment.get("comment")));
        }

        return Optional.of(metaData);
    }

    @Override
    public Optional<RDBTableMetadata> parse(String name) {
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
        Map<String, Object> res = sqlExecutor.select(template(getTableExistsSql(), param), lowerCasePropertySimpleMapWrapper);
        return res.get("total") != null && StringUtils.toInt(res.get("total")) > 0;
    }

    @Override
    @SneakyThrows
    public Set<String> getAllNames() {
        return sqlExecutor
                .select(SqlRequests.of(getAllTableSql()), list(lowerCasePropertySimpleMapWrapper))
                .stream()
                .map(map -> map.get("name"))
                .filter(Objects::nonNull)
                .map(String::valueOf)
                .map(String::toLowerCase)
                .collect(Collectors.toSet());
    }

    @Override
    public List<RDBTableMetadata> parseAll() {
        return getAllNames()
                .parallelStream()
                .map(this::doParse)
                .filter(Optional::isPresent)
                .map(Optional::get)
                .collect(Collectors.toList());
    }

    protected static LowerCasePropertySimpleMapWrapper lowerCasePropertySimpleMapWrapper = new LowerCasePropertySimpleMapWrapper();

    protected RDBColumnMetaDataWrapper columnMetaDataWrapper = new RDBColumnMetaDataWrapper();

    static class LowerCasePropertySimpleMapWrapper extends MapResultWrapper {
        @Override
        protected void doWrap(Map<String, Object> instance, String column, Object value) {
            column = column.toLowerCase();
            super.doWrap(instance, column, value);
        }
    }

    @SuppressWarnings("all")
    class RDBColumnMetaDataWrapper implements ResultWrapper<RDBColumnMetadata, RDBColumnMetadata> {

        public Class<RDBColumnMetadata> getType() {
            return RDBColumnMetadata.class;
        }

        @Override
        public RDBColumnMetadata newRowInstance() {
            return new RDBColumnMetadata();
        }

        @Override
        public void beforeWrap(ResultWrapperContext context) {

        }

        @Override
        public void completedWrap() {

        }

        @Override
        public RDBColumnMetadata getResult() {
            return null;
        }

        @Override
        public boolean completedWrapRow(int rowIndex, RDBColumnMetadata instance) {
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

        @Override
        public void wrapColumn(ColumnWrapperContext<RDBColumnMetadata> context) {
            doWrap(context.getInstance(), context.getColumnLabel(), context.getResult());
        }

        public void doWrap(RDBColumnMetadata instance, String attr, Object value) {
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
    }
}
