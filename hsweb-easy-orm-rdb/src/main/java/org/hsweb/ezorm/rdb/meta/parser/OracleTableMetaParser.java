package org.hsweb.ezorm.rdb.meta.parser;

import org.hsweb.ezorm.core.ObjectWrapper;
import org.hsweb.ezorm.rdb.executor.SqlExecutor;
import org.hsweb.ezorm.rdb.meta.RDBColumnMetaData;
import org.hsweb.ezorm.rdb.meta.RDBTableMetaData;
import org.hsweb.ezorm.rdb.meta.converter.ClobValueConverter;
import org.hsweb.ezorm.rdb.meta.converter.DateTimeConverter;
import org.hsweb.ezorm.rdb.meta.expand.SimpleMapWrapper;
import org.hsweb.ezorm.rdb.render.support.simple.SimpleSQL;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.sql.JDBCType;
import java.sql.SQLException;
import java.util.*;

// TODO: 16-10-28
public class OracleTableMetaParser implements TableMetaParser {
    private SqlExecutor sqlExecutor;

    private Logger logger = LoggerFactory.getLogger(this.getClass());

    public OracleTableMetaParser(SqlExecutor sqlExecutor) {
        this.sqlExecutor = sqlExecutor;
    }

    @Override
    public RDBTableMetaData parse(String name) {
        RDBTableMetaData metaData = new RDBTableMetaData();
        metaData.setName(name);
        metaData.setAlias(name);
        metaData.setComment("");
        String filedMetaSqlStr = "\nselect distinct(cols.column_name) as \"name\"" +
                ",cols.table_name as \"table_name\"" +
                ",cols.data_type as \"data_type\"" +
                ",cols.data_length as \"data_length\"" +
                ",cols.data_precision as \"data_precision\"" +
                ",cols.data_scale as \"data_scale\"" +
                ",acc.comments as \"comment\"" +
                ",cols.nullable as \"not-null\"" +
                ",cols.column_id from user_tab_columns cols" +
                "\nleft join all_col_comments acc on acc.column_name=cols.column_name and acc.table_name=cols.table_name" +
                "\nwhere cols.table_name=#{tableName}" +
                "\norder by cols.column_id";
        String findTableCommentSqlStr = "select comments as \"comment\" from user_tab_comments where table_name=#{tableName}";

        Map<String, Object> param = new HashMap<>();
        param.put("tableName", metaData.getName().toUpperCase());
        SimpleSQL filedMetaSql = new SimpleSQL(filedMetaSqlStr, param);
        try {
            sqlExecutor.single(new SimpleSQL(findTableCommentSqlStr, param), new SimpleMapWrapper() {
                @Override
                public void done(Map<String, Object> instance) {
                    metaData.setComment((String) instance.get("comment"));
                }
            });
            List<RDBColumnMetaData> RDBColumnMetaData = sqlExecutor.list(filedMetaSql, new FieldMetaDataWrapper());
            if (RDBColumnMetaData.isEmpty()) return null;
            RDBColumnMetaData.forEach(meta -> metaData.addColumn(meta));
        } catch (SQLException e) {
            logger.error("解析表结构{}失败", name, e);
            return null;
        }

        return metaData;
    }

    @Override
    public List<RDBTableMetaData> parseAll() throws SQLException {
        String sql = "select table_name as \"name\" from user_tab_comments where table_type='TABLE'";
        List<RDBTableMetaData> metaDatas = new LinkedList<>();
        sqlExecutor.list(new SimpleSQL(sql), new SimpleMapWrapper() {
            @Override
            public void done(Map<String, Object> instance) {
                String name = (String) instance.get("name");
                RDBTableMetaData metaData = parse(name);
                if (metaData != null)
                    metaDatas.add(metaData);
                super.done(instance);
            }
        });
        return metaDatas;
    }

    class FieldMetaDataWrapper implements ObjectWrapper<RDBColumnMetaData> {
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
            if (attr.equalsIgnoreCase("name")) {
                instance.setName(String.valueOf(value).toLowerCase());
                instance.setProperty("old-name", instance.getName());
            } else if (attr.equalsIgnoreCase("comment")) {
                instance.setComment(String.valueOf(value));
            } else {
                if (attr.toLowerCase().equals("not-null")) {
                    value = !"Y".equals(value);
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
                data_type = "varchar2";
            }
            JDBCType jdbcType = JDBCType.VARCHAR;
            Class javaType = String.class;
            switch (data_type) {
                case "varchar2":
                case "varchar":
                    data_type = data_type + "(" + len + ")";
                    jdbcType = JDBCType.VARCHAR;
                    break;
                case "number":
                    if (data_scale == 0) {
                        jdbcType = JDBCType.INTEGER;
                        javaType = Integer.class;
                        data_type = data_type + "(" + data_precision + ")";
                    } else {
                        data_type = data_type + "(" + data_precision + "," + data_scale + ")";
                        jdbcType = JDBCType.NUMERIC;
                        javaType = Double.class;
                    }
                    break;
                case "timestamp":
                case "date":
                    javaType = Date.class;
                    instance.setValueConverter(new DateTimeConverter("yyyy-MM-dd HH:mm:ss", Date.class));
                    break;
                case "clob":
                    jdbcType = JDBCType.CLOB;
                    javaType = String.class;
                    instance.setValueConverter(new ClobValueConverter());
                    break;
            }
            instance.setDataType(data_type);
            instance.setJdbcType(jdbcType);
            instance.setJavaType(javaType);
        }
    }
}
