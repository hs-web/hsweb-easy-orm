package og.hsweb.ezorm.meta.parser;

import og.hsweb.ezorm.executor.SqlExecutor;
import og.hsweb.ezorm.meta.FieldMetaData;
import og.hsweb.ezorm.meta.TableMetaData;
import og.hsweb.ezorm.meta.converter.ClobValueConverter;
import og.hsweb.ezorm.meta.converter.DateTimeConverter;
import og.hsweb.ezorm.meta.expand.ObjectWrapper;
import og.hsweb.ezorm.meta.expand.SimpleMapWrapper;
import og.hsweb.ezorm.render.support.simple.SimpleSQL;
import org.webbuilder.utils.common.DateTimeUtils;
import org.webbuilder.utils.common.StringUtils;

import java.sql.JDBCType;
import java.sql.SQLException;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by zhouhao on 16-6-5.
 */
public class OracleTableMetaParser implements TableMetaParser {
    private SqlExecutor sqlExecutor;

    public OracleTableMetaParser(SqlExecutor sqlExecutor) {
        this.sqlExecutor = sqlExecutor;
    }

    @Override
    public TableMetaData parse(String name) {
        TableMetaData metaData = new TableMetaData();
        metaData.setName(name);
        metaData.setAlias(name);
        metaData.setComment("");
        String filedMetaSqlStr = "\nselect distinct(cols.column_name) as \"name\"" +
                ",cols.table_name as \"table_name\"" +
                ",cols.data_type as \"data_type\"" +
                ",cols.data_length as \"data_length\"" +
                ",cols.data_precision as \"data_precision\"" +
                ",acc.comments as \"comment\"" +
                ",cols.column_id from user_tab_columns cols" +
                "\nleft join all_col_comments acc on acc.column_name=cols.column_name and acc.table_name=cols.table_name" +
                "\nwhere cols.table_name=#{tableName}" +
                "\norder by cols.column_id";
        String findTableCommentSqlStr = "select comments as \"comment\" from user_tab_comments where table_name=#{tableName}";

        Map<String, Object> param = new HashMap<>();
        param.put("tableName", metaData.getName().toUpperCase());
        SimpleSQL filedMetaSql = new SimpleSQL(metaData, filedMetaSqlStr, param);
        try {
            sqlExecutor.single(new SimpleSQL(metaData, findTableCommentSqlStr, param), new SimpleMapWrapper() {
                @Override
                public void done(Map<String, Object> instance) {
                    metaData.setComment((String) instance.get("comment"));
                }
            });
            List<FieldMetaData> fieldMetaData = sqlExecutor.list(filedMetaSql, new FieldMetaDataWrapper());
            fieldMetaData.forEach(meta -> metaData.addField(meta));
        } catch (SQLException e) {
            return null;
        }

        return metaData;
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
            } else if (attr.equalsIgnoreCase("comment")) {
                instance.setComment(String.valueOf(value).toLowerCase());
            } else {
                instance.setProperty(attr.toLowerCase(), value);
            }
        }

        @Override
        public void done(FieldMetaData instance) {
            String data_type = instance.getProperty("data_type").toString().toLowerCase();
            int len = instance.getProperty("data_length").toInt();
            int data_precision = instance.getProperty("data_precision").toInt();
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
                    data_type = data_type + "(" + len + "," + data_precision + ")";
                    if (data_precision == 0) {
                        jdbcType = JDBCType.INTEGER;
                        javaType = Integer.class;
                    } else {
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
