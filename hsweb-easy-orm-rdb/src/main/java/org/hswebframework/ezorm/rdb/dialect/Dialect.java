package org.hswebframework.ezorm.rdb.dialect;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.SqlExecutor;
import org.hswebframework.ezorm.rdb.meta.RDBColumnMetaData;
import org.hswebframework.ezorm.rdb.meta.parser.TableMetaParser;
import org.hswebframework.ezorm.rdb.render.SqlAppender;
import org.hswebframework.ezorm.rdb.dialect.function.SqlFunction;
import org.hswebframework.ezorm.rdb.supports.h2.H2Dialect;
import org.hswebframework.ezorm.rdb.supports.mssql.MSSQLDialect;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlDialect;
import org.hswebframework.ezorm.rdb.supports.oracle.OracleDialect;
import org.hswebframework.ezorm.rdb.supports.posgres.PGSqlDialect;
import org.hswebframework.utils.StringUtils;

import java.sql.JDBCType;

/**
 * 数据库方言
 *
 * @see DefaultDialect
 * @see MysqlDialect
 * @see OracleDialect
 * @see H2Dialect
 * @see PGSqlDialect
 * @since 1.0
 */
public interface Dialect {
    void setTermTypeMapper(String termType, TermTypeMapper mapper);

    boolean isSupportTermType(String termType);

    void setDataTypeMapper(JDBCType jdbcType, DataTypeMapper mapper);

    String getQuoteStart();

    String getQuoteEnd();

    default String quote(String keyword) {
        return getQuoteStart().concat(keyword).concat(getQuoteEnd());
    }

    SqlAppender buildCondition(String wherePrefix, Term term, RDBColumnMetaData column, String tableAlias);

    String buildDataType(RDBColumnMetaData columnMetaData);

    String doPaging(String sql, int pageIndex, int pageSize);

    String doPaging(String sql, int pageIndex, int pageSize, boolean prepare);

    SqlFunction getFunction(String name);

    SqlFunction installFunction(String name, SqlFunction function);

    boolean columnToUpperCase();

    default String buildColumnName(String tableName, String columnName) {
        if (columnName.contains(".")) {
            return columnName;
        }
        if (StringUtils.isNullOrEmpty(tableName)) {
            return StringUtils.concat(getQuoteStart(), columnToUpperCase() ? columnName.toUpperCase() : columnName, getQuoteEnd());
        }
        return StringUtils.concat(tableName, ".", getQuoteStart(), columnToUpperCase() ? columnName.toUpperCase() : columnName, getQuoteEnd());
    }

    TableMetaParser getDefaultParser(SqlExecutor sqlExecutor);

    JDBCType getJdbcType(String dataType);

    Dialect MYSQL    = new MysqlDialect();
    Dialect ORACLE   = new OracleDialect();
    Dialect H2       = new H2Dialect();
    Dialect MSSQL    = new MSSQLDialect();
    Dialect POSTGRES = new PGSqlDialect();

}
