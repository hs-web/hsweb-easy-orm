package org.hswebframework.ezorm.rdb.render.dialect;

import org.hswebframework.ezorm.rdb.executor.SqlExecutor;
import org.hswebframework.ezorm.rdb.meta.parser.OracleTableMetaParser;
import org.hswebframework.ezorm.rdb.meta.parser.TableMetaParser;
import org.hswebframework.ezorm.rdb.render.dialect.function.SqlFunction;
import org.hswebframework.ezorm.rdb.render.dialect.term.BoostTermTypeMapper;
import org.hswebframework.utils.StringUtils;

import java.sql.JDBCType;
import java.util.List;
import java.util.StringJoiner;

public class OracleDialect extends DefaultDialect {
    protected OracleDialect() {
        defaultDataTypeMapper = (meta) -> meta.getJdbcType().getName().toLowerCase();
        setDataTypeMapper(JDBCType.CHAR, (meta) -> StringUtils.concat("char(", meta.getLength(), ")"));
        setDataTypeMapper(JDBCType.NCHAR, (meta) -> StringUtils.concat("nchar(", meta.getLength(), ")"));
        setDataTypeMapper(JDBCType.VARCHAR, (meta) -> StringUtils.concat("varchar2(", meta.getLength(), ")"));
        setDataTypeMapper(JDBCType.NVARCHAR, (meta) -> StringUtils.concat("nvarchar2(", meta.getLength(), ")"));
        setDataTypeMapper(JDBCType.TIMESTAMP, (meta) -> "timestamp");
        setDataTypeMapper(JDBCType.TIME, (meta) -> "datetime");
        setDataTypeMapper(JDBCType.DATE, (meta) -> "date");
        setDataTypeMapper(JDBCType.TINYINT, (meta) -> "number(2)");
        setDataTypeMapper(JDBCType.SMALLINT, (meta) -> "number(5)");
        setDataTypeMapper(JDBCType.INTEGER, (meta) -> "integer");
        setDataTypeMapper(JDBCType.DOUBLE, (meta) -> "double");
        setDataTypeMapper(JDBCType.CLOB, (meta) -> "clob");
        setDataTypeMapper(JDBCType.BLOB, (meta) -> "blob");
        setDataTypeMapper(JDBCType.BINARY, (meta) -> "blob");
        setDataTypeMapper(JDBCType.NUMERIC, (meta) -> StringUtils.concat("number(", meta.getPrecision(), ",", meta.getScale(), ")"));
        setDataTypeMapper(JDBCType.DECIMAL, (meta) -> StringUtils.concat("number(", meta.getPrecision(), ",", meta.getScale(), ")"));
        setDataTypeMapper(JDBCType.BIGINT, (meta) -> "bigint");
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
        return new StringBuilder()
                .append("SELECT * FROM ( SELECT row_.*, rownum rownum_ FROM (")
                .append(sql)
                .append(") row_ )")
                .append("WHERE rownum_ <= ")
                .append(pageSize * (pageIndex + 1))
                .append(" AND rownum_ > ")
                .append(pageSize * pageIndex).toString();
    }

    @Override
    public boolean columnToUpperCase() {
        return true;
    }

    @Override
    public TableMetaParser getDefaultParser(SqlExecutor sqlExecutor) {
        return new OracleTableMetaParser(sqlExecutor);
    }
}
