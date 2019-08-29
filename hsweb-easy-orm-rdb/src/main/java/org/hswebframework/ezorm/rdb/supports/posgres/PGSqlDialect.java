package org.hswebframework.ezorm.rdb.supports.posgres;

import org.hswebframework.ezorm.rdb.dialect.DefaultDialect;
import org.hswebframework.ezorm.rdb.dialect.function.SqlFunction;
import org.hswebframework.ezorm.rdb.dialect.AutomaticConvertValueTermTypeMapper;
import org.hswebframework.utils.StringUtils;

import java.sql.JDBCType;
import java.util.List;
import java.util.StringJoiner;

/**
 * @author zhouhao
 * @since 3.0
 */
public class PGSqlDialect extends DefaultDialect {


    public PGSqlDialect() {
        defaultDataTypeMapper = (meta) -> meta.getJdbcType().getName().toLowerCase();
        setDataTypeMapper(JDBCType.CHAR, (meta) -> StringUtils.concat("char(", meta.getLength(), ")"));
        setDataTypeMapper(JDBCType.VARCHAR, (meta) -> StringUtils.concat("varchar(", meta.getLength(), ")"));
        setDataTypeMapper(JDBCType.TIMESTAMP, (meta) -> "timestamp");
        setDataTypeMapper(JDBCType.TIME, (meta) -> "time");
        setDataTypeMapper(JDBCType.DATE, (meta) -> "date");
        setDataTypeMapper(JDBCType.CLOB, (meta) -> "text");
        setDataTypeMapper(JDBCType.LONGVARBINARY, (meta) -> "bytea");
        setDataTypeMapper(JDBCType.LONGVARCHAR, (meta) -> "text");
        setDataTypeMapper(JDBCType.BLOB, (meta) -> "bytea");
        setDataTypeMapper(JDBCType.BIGINT, (meta) -> "bigint");
        setDataTypeMapper(JDBCType.DOUBLE, (meta) -> "double");
        setDataTypeMapper(JDBCType.INTEGER, (meta) -> "integer");
        setDataTypeMapper(JDBCType.NUMERIC, (meta) -> StringUtils.concat("numeric(", meta.getPrecision(), ",", meta.getScale(), ")"));
        setDataTypeMapper(JDBCType.DECIMAL, (meta) -> StringUtils.concat("decimal(", meta.getPrecision(), ",", meta.getScale(), ")"));
        setDataTypeMapper(JDBCType.TINYINT, (meta) -> "smallint");
        setDataTypeMapper(JDBCType.BIGINT, (meta) -> "bigint");
        setDataTypeMapper(JDBCType.OTHER, (meta) -> "other");

        setJdbcTypeMapping("int", JDBCType.INTEGER);
        setJdbcTypeMapping("int2", JDBCType.INTEGER);
        setJdbcTypeMapping("int4", JDBCType.INTEGER);

        setJdbcTypeMapping("float8", JDBCType.DOUBLE);
        setJdbcTypeMapping("money", JDBCType.DOUBLE);
        setJdbcTypeMapping("bool", JDBCType.BOOLEAN);

        setJdbcTypeMapping("character", JDBCType.VARCHAR);
        setJdbcTypeMapping("xml", JDBCType.VARCHAR);

        setJdbcTypeMapping("year", JDBCType.TIME);
        setJdbcTypeMapping("datetime", JDBCType.TIMESTAMP);
        setJdbcTypeMapping("text", JDBCType.CLOB);

        installFunction(SqlFunction.concat, param -> {
            List<Object> listParam = AutomaticConvertValueTermTypeMapper.convertList(param.getParam());
            StringJoiner joiner = new StringJoiner("||");
            listParam.stream()
                    .map(String::valueOf)
                    .forEach(joiner::add);
            return joiner.toString();
        });

        installFunction(SqlFunction.bitand, param -> {
            List<Object> listParam = AutomaticConvertValueTermTypeMapper.convertList(param.getParam());
            if (listParam.isEmpty()) {
                throw new IllegalArgumentException("[BITAND]参数不能为空");
            }
            StringJoiner joiner = new StringJoiner("&");
            listParam.stream().map(String::valueOf).forEach(joiner::add);
            return joiner.toString();
        });
    }

    @Override
    public String getQuoteStart() {
        return "\"";
    }

    @Override
    public String getQuoteEnd() {
        return "\"";
    }

    @Override
    public String doPaging(String sql, int pageIndex, int pageSize, boolean prepare) {
        if (prepare) {
            return sql + " limit #{pageSize} offset #{pageSize}*#{pageIndex}";
        }
        return new StringBuilder(sql)
                .append(" limit ")
                .append(pageSize)
                .append(" offset ")
                .append(pageSize * pageIndex)
                .toString();
    }

    @Override
    public boolean columnToUpperCase() {
        return false;
    }
}
