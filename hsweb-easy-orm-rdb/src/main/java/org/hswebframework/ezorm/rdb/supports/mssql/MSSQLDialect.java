package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.rdb.dialect.DefaultDialect;
import org.hswebframework.ezorm.rdb.dialect.function.SqlFunction;
import org.hswebframework.ezorm.rdb.dialect.AutomaticConvertValueTermTypeMapper;
import org.hswebframework.utils.StringUtils;

import java.sql.JDBCType;
import java.util.List;
import java.util.StringJoiner;

/**
 * @author zhouhao
 */
public class MSSQLDialect extends DefaultDialect {

    public MSSQLDialect() {
        defaultDataTypeMapper = (meta) -> meta.getJdbcType().getName().toLowerCase();
        setDataTypeMapper(JDBCType.CHAR, (meta) -> StringUtils.concat("char(", meta.getLength(), ")"));
        setDataTypeMapper(JDBCType.NCHAR, (meta) -> StringUtils.concat("nchar(", meta.getLength(), ")"));
        setDataTypeMapper(JDBCType.VARCHAR, (meta) -> StringUtils.concat("varchar(", meta.getLength(), ")"));
        setDataTypeMapper(JDBCType.NVARCHAR, (meta) -> StringUtils.concat("nvarchar(", meta.getLength(), ")"));
        setDataTypeMapper(JDBCType.TIMESTAMP, (meta) -> "datetime");
        setDataTypeMapper(JDBCType.TIME, (meta) -> "time");
        setDataTypeMapper(JDBCType.DATE, (meta) -> "date");
        setDataTypeMapper(JDBCType.CLOB, (meta) -> "text");
        setDataTypeMapper(JDBCType.LONGVARBINARY, (meta) -> "varbinary(max)");
        setDataTypeMapper(JDBCType.LONGVARCHAR, (meta) -> "text");
        setDataTypeMapper(JDBCType.BLOB, (meta) -> "varbinary(max)");
        setDataTypeMapper(JDBCType.BIGINT, (meta) -> "bigint");
        setDataTypeMapper(JDBCType.DOUBLE, (meta) -> "double");
        setDataTypeMapper(JDBCType.INTEGER, (meta) -> "int");
        setDataTypeMapper(JDBCType.NUMERIC, (meta) -> StringUtils.concat("numeric(", meta.getPrecision(), ",", meta.getScale(), ")"));
        setDataTypeMapper(JDBCType.DECIMAL, (meta) -> StringUtils.concat("numeric(", meta.getPrecision(), ",", meta.getScale(), ")"));
        setDataTypeMapper(JDBCType.TINYINT, (meta) -> "tinyint");
        setDataTypeMapper(JDBCType.BIGINT, (meta) -> "bigint");
        setDataTypeMapper(JDBCType.OTHER, (meta) -> "other");
        setDataTypeMapper(JDBCType.REAL, (meta) -> "real");


        setJdbcTypeMapping("bigint", JDBCType.BIGINT);
        setJdbcTypeMapping("binary", JDBCType.BINARY);
        setJdbcTypeMapping("bit", JDBCType.BIT);
        setJdbcTypeMapping("char", JDBCType.CHAR);
        setJdbcTypeMapping("datetime", JDBCType.TIMESTAMP);
        setJdbcTypeMapping("decimal", JDBCType.DECIMAL);
        setJdbcTypeMapping("float", JDBCType.FLOAT);
        setJdbcTypeMapping("image", JDBCType.LONGVARBINARY);
        setJdbcTypeMapping("int", JDBCType.INTEGER);
        setJdbcTypeMapping("money", JDBCType.DECIMAL);
        setJdbcTypeMapping("nchar", JDBCType.CHAR);
        setJdbcTypeMapping("ntext", JDBCType.LONGVARCHAR);
        setJdbcTypeMapping("numeric", JDBCType.NUMERIC);
        setJdbcTypeMapping("nvarchar", JDBCType.VARCHAR);
        setJdbcTypeMapping("real", JDBCType.REAL);
        setJdbcTypeMapping("smalldatetime", JDBCType.TIMESTAMP);
        setJdbcTypeMapping("smallint", JDBCType.SMALLINT);
        setJdbcTypeMapping("smallmoney", JDBCType.DECIMAL);
        setJdbcTypeMapping("sql_variant", JDBCType.VARCHAR);
        setJdbcTypeMapping("sysname", JDBCType.VARCHAR);
        setJdbcTypeMapping("text", JDBCType.LONGVARCHAR);
        setJdbcTypeMapping("timestamp", JDBCType.BINARY);
        setJdbcTypeMapping("tinyint", JDBCType.TINYINT);
        setJdbcTypeMapping("uniqueidentifier", JDBCType.CHAR);
        setJdbcTypeMapping("varbinary", JDBCType.VARBINARY);
        setJdbcTypeMapping("varchar", JDBCType.VARCHAR);

//        installFunction(SqlFunction.concat, param -> {
//            List<Object> listParam = AutomaticConvertValueTermTypeMapper.convertList(param.getParam());
//            StringJoiner joiner = new StringJoiner(",", "concat(", ")");
//            listParam.stream().map(String::valueOf).forEach(joiner::add);
//            return joiner.toString();
//        });
//        installFunction(SqlFunction.bitand, param -> {
//            List<Object> listParam = AutomaticConvertValueTermTypeMapper.convertList(param.getParam());
//            if (listParam.size() != 2) {
//                throw new IllegalArgumentException("[bitand]参数长度必须为2");
//            }
//            StringJoiner joiner = new StringJoiner(",", "bitand(", ")");
//            listParam.stream().map(String::valueOf).forEach(joiner::add);
//            return joiner.toString();
//        });

    }

    @Override
    public String getQuoteStart() {
        return "[";
    }

    @Override
    public String getQuoteEnd() {
        return "]";
    }

    @Override
    public String doPaging(String sql, int pageIndex, int pageSize, boolean prepare) {
        if (!sql.contains("order") && !sql.contains("ORDER")) {
            sql = sql.concat(" order by 1");
        }
        if (prepare) {
            return sql + " OFFSET #{pageSize}*#{pageIndex}  ROWS FETCH NEXT #{pageSize} ROWS ONLY";
        }
        return sql.concat(" OFFSET " + (pageIndex * pageSize) + " ROWS FETCH NEXT " + pageSize + " ROWS ONLY");
    }

    @Override
    public boolean columnToUpperCase() {
        return false;
    }


}
