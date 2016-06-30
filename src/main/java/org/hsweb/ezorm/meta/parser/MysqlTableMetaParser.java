package org.hsweb.ezorm.meta.parser;

import org.hsweb.ezorm.executor.SqlExecutor;
import org.hsweb.ezorm.meta.FieldMetaData;
import org.hsweb.ezorm.meta.TableMetaData;
import org.hsweb.ezorm.meta.converter.ClobValueConverter;
import org.hsweb.ezorm.meta.converter.DateTimeConverter;
import org.hsweb.ezorm.meta.converter.NumberValueConverter;
import org.hsweb.ezorm.meta.expand.ObjectWrapper;
import org.hsweb.ezorm.meta.expand.SimpleMapWrapper;
import org.hsweb.ezorm.render.support.simple.SimpleSQL;

import java.sql.JDBCType;
import java.sql.SQLException;
import java.util.*;

/**
 * Created by zhouhao on 16-6-5.
 */
public class MysqlTableMetaParser implements TableMetaParser {
    private SqlExecutor sqlExecutor;

    public MysqlTableMetaParser(SqlExecutor sqlExecutor) {
        this.sqlExecutor = sqlExecutor;
    }

    @Override
    public TableMetaData parse(String name) {
        TableMetaData metaData = new TableMetaData();
        metaData.setName(name);
        metaData.setAlias(name);
        metaData.setComment("");

        String filedMetaSqlStr = " select\n" +
                "        COLUMN_NAME as `name`,\n" +
                "        DATA_TYPE as `data_type`,\n" +
                "        CHARACTER_MAXIMUM_LENGTH as `data_length`,\n" +
                "        NUMERIC_PRECISION as `data_precision`,\n" +
                "        COLUMN_COMMENT as `comment`,\n" +
                "        IS_NULLABLE as `not-null`\n" +
                "        from information_schema.columns where table_name=#{tableName}";
        String findTableCommentSqlStr = " select\n" +
                "        table_comment as `comment`\n" +
                "        from information_schema.tables where table_name=#{tableName}";

        Map<String, Object> param = new HashMap<>();
        param.put("tableName", metaData.getName().toLowerCase());
        SimpleSQL filedMetaSql = new SimpleSQL(filedMetaSqlStr, param);
        try {
            sqlExecutor.single(new SimpleSQL(findTableCommentSqlStr, param), new SimpleMapWrapper() {
                @Override
                public void done(Map<String, Object> instance) {
                    metaData.setComment((String) instance.get("comment"));
                }
            });
            List<FieldMetaData> fieldMetaData = sqlExecutor.list(filedMetaSql, new FieldMetaDataWrapper());
            if (fieldMetaData.isEmpty()) return null;
            fieldMetaData.forEach(meta -> metaData.addField(meta));
        } catch (SQLException e) {
            e.printStackTrace();
            return null;
        }
        return metaData;
    }

    @Override
    public List<TableMetaData> parseAll() throws SQLException {
        String sql = "SELECT table_name as `name` from information_schema.`TABLES` where table_schema=database()";
        List<TableMetaData> metaDatas = new LinkedList<>();
        sqlExecutor.list(new SimpleSQL(sql), new SimpleMapWrapper() {
            @Override
            public void done(Map<String, Object> instance) {
                String name = (String) instance.get("name");
                TableMetaData metaData = parse(name);
                metaDatas.add(metaData);
                super.done(instance);
            }
        });
        return metaDatas;
    }

    class FieldMetaDataWrapper implements ObjectWrapper<FieldMetaData> {

        @Override
        public FieldMetaData newInstance() {
            return new FieldMetaData();
        }

        @Override
        public void wrapper(FieldMetaData instance, int index, String attr, Object value) {
            if (attr.equalsIgnoreCase("name")) {
                instance.setName(String.valueOf(value).toLowerCase());
                instance.setProperty("old-name", instance.getName());
            } else if (attr.equalsIgnoreCase("comment")) {
                instance.setComment(String.valueOf(value).toLowerCase());
            } else {
                if (attr.toLowerCase().equals("not-null")) {
                    value = !"yes".equals(String.valueOf(value).toLowerCase());
                }
                instance.setProperty(attr.toLowerCase(), value);
            }
        }

        @Override
        public void done(FieldMetaData instance) {
            String data_type = instance.getProperty("data_type").toString().toLowerCase();
            int len = instance.getProperty("data_length").toInt();
            int data_precision = instance.getProperty("data_precision").toInt();
            if (data_type == null) {
                data_type = "varchar";
            }
            JDBCType jdbcType = JDBCType.VARCHAR;
            Class javaType = String.class;
            switch (data_type) {
                case "text":
                case "varchar":
                    data_type = data_type + "(" + len + ")";
                    jdbcType = JDBCType.VARCHAR;
                    break;
                case "tinyint":
                case "int":
                    data_type = data_type + "(" + len + ")";
                    jdbcType = JDBCType.INTEGER;
                    javaType = Integer.class;
                    instance.setValueConverter(new NumberValueConverter(javaType));
                    break;
                case "decimal":
                case "float":
                case "double":
                    data_type = data_type + "(" + len + "," + data_precision + ")";
                    jdbcType = JDBCType.NUMERIC;
                    javaType = Double.class;
                    instance.setValueConverter(new NumberValueConverter(javaType));
                    break;
                case "datetime":
                case "timestamp":
                case "date":
                    javaType = Date.class;
                    instance.setValueConverter(new DateTimeConverter("yyyy-MM-dd HH:mm:ss", Date.class));
                    break;
            }
            instance.setDataType(data_type);
            instance.setJdbcType(jdbcType);
            instance.setJavaType(javaType);
        }
    }
}
