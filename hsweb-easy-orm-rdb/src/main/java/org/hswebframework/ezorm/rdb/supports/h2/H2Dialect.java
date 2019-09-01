package org.hswebframework.ezorm.rdb.supports.h2;

import org.hswebframework.ezorm.rdb.dialect.DefaultDialect;
import org.hswebframework.ezorm.rdb.dialect.function.SqlFunction;
import org.hswebframework.ezorm.rdb.dialect.AutomaticConvertValueTermTypeMapper;
import org.hswebframework.utils.StringUtils;

import java.sql.JDBCType;
import java.util.List;
import java.util.StringJoiner;

public class H2Dialect extends DefaultDialect {
    public H2Dialect() {
        defaultDataTypeMapper = (meta) -> meta.getJdbcType().getName().toLowerCase();
        setDataTypeMapper(JDBCType.VARCHAR, (meta) -> StringUtils.concat("varchar(", meta.getLength(), ")"));
        setDataTypeMapper(JDBCType.TIMESTAMP, (meta) -> "timestamp");
        setDataTypeMapper(JDBCType.SMALLINT, (meta) -> "smallint");
        setDataTypeMapper(JDBCType.BIGINT, (meta) -> "bigint");
        setDataTypeMapper(JDBCType.TIME, (meta) -> "timestamp");
        setDataTypeMapper(JDBCType.DATE, (meta) -> "date");
        setDataTypeMapper(JDBCType.CLOB, (meta) -> "clob");
        setDataTypeMapper(JDBCType.BLOB, (meta) -> "blob");
        setDataTypeMapper(JDBCType.INTEGER, (meta) -> "int");
        setDataTypeMapper(JDBCType.NUMERIC, (meta) -> StringUtils.concat("decimal(", meta.getPrecision(), ",", meta.getScale(), ")"));
        setDataTypeMapper(JDBCType.TINYINT, (meta) -> "tinyint");
        setDataTypeMapper(JDBCType.DECIMAL, (meta) -> StringUtils.concat("decimal(", meta.getPrecision(), ",", meta.getScale(), ")"));
        setDataTypeMapper(JDBCType.OTHER, (meta) -> "other");

//        installFunction(SqlFunction.concat, param -> {
//            List<Object> listParam = AutomaticConvertValueTermTypeMapper.convertList(param.getParam());
//            StringJoiner joiner = new StringJoiner("||");
//            listParam.stream().map(String::valueOf).forEach(joiner::add);
//            return joiner.toString();
//        });
//
//        installFunction(SqlFunction.bitand, param -> {
//            List<Object> listParam = AutomaticConvertValueTermTypeMapper.convertList(param.getParam());
//            if (listParam.size() != 2) {
//                throw new IllegalArgumentException("[BITAND]参数长度必须为2");
//            }
//            StringJoiner joiner = new StringJoiner(",", "BITAND(", ")");
//            listParam.stream().map(String::valueOf).forEach(joiner::add);
//            return joiner.toString();
//        });

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
            return sql + " paging #{pageSize}*#{pageIndex} , #{pageSize}";
        }
        return new StringBuilder(sql)
                .append(" paging ")
                .append(pageSize * pageIndex)
                .append(",")
                .append(pageSize)
                .toString();
    }

    @Override
    public boolean columnToUpperCase() {
        return true;
    }


}
