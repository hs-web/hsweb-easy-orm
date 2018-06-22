package org.hswebframework.ezorm.rdb.render.dialect;

import org.hswebframework.ezorm.rdb.meta.parser.H2TableMetaParser;
import org.hswebframework.ezorm.rdb.meta.parser.TableMetaParser;
import org.hswebframework.ezorm.rdb.render.dialect.function.SqlFunction;
import org.hswebframework.ezorm.rdb.render.dialect.term.BoostTermTypeMapper;
import org.hswebframework.utils.StringUtils;
import org.hswebframework.ezorm.rdb.executor.SqlExecutor;

import java.sql.JDBCType;
import java.util.List;
import java.util.StringJoiner;

public class H2Dialect extends DefaultDialect {
    protected H2Dialect() {
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

        installFunction(SqlFunction.concat, param -> {
            List<Object> listParam = BoostTermTypeMapper.convertList(param.getParam());
            StringJoiner joiner = new StringJoiner("||");
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
    public String doPaging(String sql, int pageIndex, int pageSize) {
        return new StringBuilder(sql)
                .append(" limit ")
                .append(pageSize * pageIndex)
                .append(",")
                .append(pageSize).toString();
    }

    @Override
    public boolean columnToUpperCase() {
        return true;
    }

    @Override
    public TableMetaParser getDefaultParser(SqlExecutor sqlExecutor) {
        return new H2TableMetaParser(sqlExecutor);
    }
}
