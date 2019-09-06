package org.hswebframework.ezorm.rdb.metadata.dialect;

import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatureType;
import org.hswebframework.ezorm.rdb.supports.h2.H2Dialect;
import org.hswebframework.ezorm.rdb.supports.mssql.MSSQLDialect;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlDialect;
import org.hswebframework.ezorm.rdb.supports.oracle.OracleDialect;
import org.hswebframework.ezorm.rdb.supports.posgres.PostgreSQLDialect;
import org.hswebframework.utils.StringUtils;

import java.sql.JDBCType;

/**
 * 数据库方言
 *
 * @see DefaultDialect
 * @see MysqlDialect
 * @see OracleDialect
 * @see H2Dialect
 * @see PostgreSQLDialect
 * @since 1.0
 */
public interface Dialect extends Feature {

    @Override
    default RDBFeatureType getType() {
        return RDBFeatureType.dialect;
    }

    void addDataTypeMapper(JDBCType jdbcType, DataTypeMapper mapper);

    String buildDataType(RDBColumnMetadata columnMetaData);

    String getQuoteStart();

    String getQuoteEnd();

    String clearQuote(String string);

    default String quote(String keyword, boolean changeCase) {
        if (keyword.startsWith(getQuoteStart()) && keyword.endsWith(getQuoteEnd())) {
            return keyword;
        }
        return getQuoteStart().concat(isColumnToUpperCase() && changeCase ? keyword.toUpperCase() : keyword).concat(getQuoteEnd());
    }

    default String quote(String keyword) {
        return quote(keyword, true);
    }

    boolean isColumnToUpperCase();

    default String buildColumnFullName(String tableName, String columnName) {
        if (columnName.contains(".")) {
            return columnName;
        }
        if (StringUtils.isNullOrEmpty(tableName)) {
            return StringUtils.concat(getQuoteStart(), isColumnToUpperCase() ? columnName.toUpperCase() : columnName, getQuoteEnd());
        }
        return StringUtils.concat(tableName, ".", getQuoteStart(), isColumnToUpperCase() ? columnName.toUpperCase() : columnName, getQuoteEnd());
    }


    JDBCType getJdbcType(String dataType);

    Dialect MYSQL = new MysqlDialect();
    Dialect ORACLE = new OracleDialect();
    Dialect H2 = new H2Dialect();
    Dialect MSSQL = new MSSQLDialect();
    Dialect POSTGRES = new PostgreSQLDialect();

}
