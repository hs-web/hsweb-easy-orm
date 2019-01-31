package org.hswebframework.ezorm.rdb.render.dialect;

import org.hswebframework.ezorm.rdb.executor.SqlExecutor;
import org.hswebframework.ezorm.rdb.meta.parser.TableMetaParser;
import org.hswebframework.ezorm.rdb.render.dialect.function.SqlFunction;
import org.hswebframework.ezorm.rdb.render.dialect.term.BoostTermTypeMapper;
import org.hswebframework.utils.StringUtils;
import org.hswebframework.ezorm.rdb.meta.parser.SqlServer2012TableMetaParser;

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

        installFunction(SqlFunction.concat, param -> {
            List<Object> listParam = BoostTermTypeMapper.convertList(param.getParam());
            StringJoiner joiner = new StringJoiner(",", "concat(", ")");
            listParam.stream().map(String::valueOf).forEach(joiner::add);
            return joiner.toString();
        });
        installFunction(SqlFunction.bitand, param -> {
            List<Object> listParam = BoostTermTypeMapper.convertList(param.getParam());
            if (listParam.size() != 2) {
                throw new IllegalArgumentException("[bitand]参数长度必须为2");
            }
            StringJoiner joiner = new StringJoiner(",", "bitand(", ")");
            listParam.stream().map(String::valueOf).forEach(joiner::add);
            return joiner.toString();
        });

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
    public String doPaging(String sql, int pageIndex, int pageSize,boolean prepare) {
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

    @Override
    public TableMetaParser getDefaultParser(SqlExecutor sqlExecutor) {
        return new SqlServer2012TableMetaParser(sqlExecutor);
    }
}
