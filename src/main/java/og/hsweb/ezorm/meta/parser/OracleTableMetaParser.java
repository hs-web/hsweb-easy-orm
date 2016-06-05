package og.hsweb.ezorm.meta.parser;

import og.hsweb.ezorm.executor.SqlExecutor;
import og.hsweb.ezorm.meta.FieldMetaData;
import og.hsweb.ezorm.meta.TableMetaData;
import og.hsweb.ezorm.meta.converter.ClobValueConverter;
import og.hsweb.ezorm.meta.converter.DateTimeConverter;
import og.hsweb.ezorm.meta.expand.ObjectWrapper;
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
    private String oracleUser;

    public OracleTableMetaParser(SqlExecutor sqlExecutor, String oracleUser) {
        this.sqlExecutor = sqlExecutor;
        this.oracleUser = oracleUser;
    }

    @Override
    public TableMetaData parse(String name) {
        TableMetaData metaData = new TableMetaData();
        metaData.setName(name);
        metaData.setAlias(name);
        metaData.setComment("");
        String filedMetaSql = "\nselect distinct(cols.column_name) as \"name\"" +
                ",cols.table_name" +
                ",cols.data_type" +
                ",cols.data_length" +
                ",cols.data_precision" +
                ",acc.comments as \"comment\"" +
                ",cols.column_id  from cols\n" +
                " left join all_col_comments acc on acc.column_name=cols.COLUMN_NAME and acc.TABLE_NAME=cols.TABLE_NAME\n" +
                " where cols.table_name=upper(#{tablename}) and acc.owner=#{oracleUser}\n" +
                " order by cols.column_id\n";
        Map<String, Object> param = new HashMap<>();
        param.put("tablename", metaData.getName().toUpperCase());
        param.put("oracleUser", oracleUser.toUpperCase());
        SimpleSQL simpleSQL = new SimpleSQL(metaData, filedMetaSql, param);
        List<FieldMetaData> fieldMetaData;
        try {
            fieldMetaData = sqlExecutor.list(simpleSQL, new FieldMetaDataWrapper());
        } catch (SQLException e) {
            return null;
        }
        fieldMetaData.forEach(meta -> metaData.addField(meta));
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
//                if (instance.getName().equals("u_id")) {
//                    instance.setAlias("id");
//                } else {
//                    instance.setAlias(StringUtils.underScoreCase2CamelCase(instance.getName()));
//                }
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
