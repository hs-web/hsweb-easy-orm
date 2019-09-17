package org.hswebframework.ezorm.rdb.supports.commons;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.metadata.parser.IndexMetadataParser;
import org.hswebframework.ezorm.rdb.metadata.parser.TableMetadataParser;

import java.math.BigDecimal;
import java.sql.JDBCType;
import java.util.*;
import java.util.stream.Collectors;

import static org.hswebframework.ezorm.rdb.executor.SqlRequests.template;
import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.*;

public abstract class RDBTableMetadataParser implements TableMetadataParser {

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

    protected RDBSchemaMetadata schema;

    protected SyncSqlExecutor sqlExecutor;

    protected Map<JDBCType, Class> javaTypeMap = new HashMap<>();

    protected Dialect getDialect() {
        return schema.getDialect();
    }

    protected abstract String getTableMetaSql(String name);

    protected abstract String getTableCommentSql(String name);

    protected abstract String getAllTableSql();

    protected abstract String getTableExistsSql();

    public RDBTableMetadataParser(RDBSchemaMetadata schema) {
        this.schema = schema;
    }

    protected SyncSqlExecutor getSqlExecutor() {
        if (this.sqlExecutor == null) {
            this.sqlExecutor = schema.<SyncSqlExecutor>findFeature(SyncSqlExecutor.id)
                    .orElseThrow(() -> new UnsupportedOperationException("unsupported SyncSqlExecutor"));
        }
        return this.sqlExecutor;
    }

    protected RDBTableMetadata createTable(String name) {
        return schema.newTable(name);
    }

    @SneakyThrows
    protected Optional<RDBTableMetadata> doParse(String name) {
        RDBTableMetadata metaData = createTable(name);
        metaData.setName(name);
        metaData.setAlias(name);
        Map<String, Object> param = new HashMap<>();
        param.put("table", name);
        param.put("schema", schema.getName());

        //列
        List<RDBColumnMetadata> metaDataList = getSqlExecutor().select(template(getTableMetaSql(name), param), list(columnMetaDataWrapper));
        metaDataList.forEach(metaData::addColumn);
        //说明
        Map<String, Object> comment = getSqlExecutor().select(template(getTableCommentSql(name), param), singleMap());
        if (null != comment && comment.get("comment") != null) {
            metaData.setComment(String.valueOf(comment.get("comment")));
        }
        //加载索引
        schema.<IndexMetadataParser>findFeature(IndexMetadataParser.id)
                .map(parser -> parser.parseTableIndex(name))
                .ifPresent(indexes -> indexes.forEach(metaData::addIndex));

        return Optional.of(metaData);
    }

    @Override
    @SuppressWarnings("all")
    public Optional<RDBTableMetadata> parseByName(String name) {
        if (!tableExists(name)) {
            return Optional.empty();
        }
        return doParse(name);
    }

    @Override
    @SneakyThrows
    public boolean tableExists(String name) {
        Map<String, Object> param = new HashMap<>();
        param.put("table", name);
        param.put("schema", schema.getName());
        return getSqlExecutor()
                .select(template(getTableExistsSql(), param),
                        optional(column("total", res -> res != null && ((Number) res).intValue() > 0)))
                .orElse(false);

    }

    @Override
    @SneakyThrows
    public List<String> parseAllTableName() {
        return getSqlExecutor()
                .select(SqlRequests.template(getAllTableSql(), Collections.singletonMap("schema", schema.getName())), list(column("name", String::valueOf)));
    }

    @Override
    @SuppressWarnings("all")
    public List<RDBTableMetadata> parseAll() {
        return parseAllTableName()
                .parallelStream()
                .map(this::doParse)
                .filter(Optional::isPresent)
                .map(Optional::get)
                .collect(Collectors.toList());
    }

    protected RDBColumnMetaDataWrapper columnMetaDataWrapper = new RDBColumnMetaDataWrapper();

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
        public RDBColumnMetadata getResult() {
            return null;
        }

        @Override
        public boolean completedWrapRow(RDBColumnMetadata instance) {
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
            doWrap(context.getRowInstance(), context.getColumnLabel(), context.getResult());
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
