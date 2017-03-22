package org.hsweb.ezorm.rdb.meta.parser;

import org.hsweb.commons.StringUtils;
import org.hsweb.ezorm.core.ObjectWrapper;
import org.hsweb.ezorm.rdb.executor.SqlExecutor;
import org.hsweb.ezorm.rdb.meta.RDBColumnMetaData;
import org.hsweb.ezorm.rdb.meta.RDBTableMetaData;
import org.hsweb.ezorm.rdb.meta.expand.SimpleMapWrapper;
import org.hsweb.ezorm.rdb.render.dialect.Dialect;
import org.hsweb.ezorm.rdb.render.support.simple.SimpleSQL;

import java.sql.JDBCType;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

public abstract class AbstractTableMetaParser implements TableMetaParser {
    Map<String, JDBCType> jdbcTypeMap = new HashMap<>();
    Map<JDBCType, Class>  javaTypeMap = new HashMap<>();
    protected SqlExecutor sqlExecutor;

    abstract Dialect getDialect();

    public AbstractTableMetaParser(SqlExecutor sqlExecutor) {
        this.sqlExecutor = sqlExecutor;
    }

    abstract String getTableMetaSql(String tname);

    abstract String getTableCommentSql(String tname);

    abstract String getAllTableSql();

    abstract String getTableExistsSql();

    @Override
    public boolean tableExists(String name) {
        try {
            Map<String, Object> param = new HashMap<>();
            param.put("table", name);
            Map<String, Object> res = sqlExecutor.single(new SimpleSQL(getTableExistsSql(), param), new LowerCasePropertySimpleMapWrapper());
            return res.get("total") != null && StringUtils.toInt(res.get("total")) > 0;
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public RDBTableMetaData parse(String name) {
        if (!tableExists(name)) return null;
        RDBTableMetaData metaData = new RDBTableMetaData();
        metaData.setName(name);
        metaData.setAlias(name);
        Map<String, Object> param = new HashMap<>();
        param.put("table", name);
        try {
            List<RDBColumnMetaData> metaDatas = sqlExecutor.list(new SimpleSQL(getTableMetaSql(name), param), new RDBColumnMetaDataWrapper());
            metaDatas.forEach(metaData::addColumn);
            Map<String, Object> comment = sqlExecutor.single(new SimpleSQL(getTableCommentSql(name), param), new LowerCasePropertySimpleMapWrapper());
            if (null != comment && comment.get("comment") != null) {
                metaData.setComment(String.valueOf(comment.get("comment")));
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
        return metaData;
    }

    @Override
    public List<RDBTableMetaData> parseAll() throws SQLException {
        List<Map<String, Object>> tables = sqlExecutor.list(new SimpleSQL(getAllTableSql()), new LowerCasePropertySimpleMapWrapper());
        return tables.stream()
                .map(map -> (String) map.get("name"))
                .filter(Objects::nonNull)
                .map(this::parse).filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    class LowerCasePropertySimpleMapWrapper extends SimpleMapWrapper {
        @Override
        public void wrapper(Map<String, Object> instance, int index, String attr, Object value) {
            attr = attr.toLowerCase();
            super.wrapper(instance, index, attr, value);
        }
    }

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
        public void done(RDBColumnMetaData instance) {
            String data_type = instance.getProperty("data_type").toString().toLowerCase();
            int len = instance.getProperty("data_length").toInt();
            int data_precision = instance.getProperty("data_precision").toInt();
            int data_scale = instance.getProperty("data_scale").toInt();
            if (data_type == null) {
                data_type = "varchar";
            }
            instance.setLength(len);
            instance.setPrecision(data_precision);
            instance.setScale(data_scale);

            JDBCType jdbcType;
            try {
                jdbcType = JDBCType.valueOf(data_type.toUpperCase());
            } catch (Exception e) {
                jdbcType = jdbcTypeMap.get(data_type);
            }
            Class javaType = javaTypeMap.get(jdbcType);
            instance.setJdbcType(jdbcType);
            instance.setJavaType(javaType);
            instance.setDataType(getDialect().buildDataType(instance));
        }
    }
}
