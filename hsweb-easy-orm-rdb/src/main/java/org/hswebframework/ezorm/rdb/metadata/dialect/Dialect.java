package org.hswebframework.ezorm.rdb.metadata.dialect;

import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatureType;
import org.hswebframework.ezorm.rdb.supports.clickhouse.ClickhouseDialect;
import org.hswebframework.ezorm.rdb.supports.h2.H2Dialect;
import org.hswebframework.ezorm.rdb.supports.mssql.SqlServerDialect;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlDialect;
import org.hswebframework.ezorm.rdb.supports.oracle.OracleDialect;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlDialect;
import org.hswebframework.ezorm.core.utils.StringUtils;

import java.sql.SQLType;
import java.util.Optional;

/**
 * 数据库方言
 *
 * @see DefaultDialect
 * @see MysqlDialect
 * @see OracleDialect
 * @see H2Dialect
 * @see PostgresqlDialect
 * @since 1.0
 */
public interface Dialect extends Feature {

    @Override
    default RDBFeatureType getType() {
        return RDBFeatureType.dialect;
    }

    void addDataTypeBuilder(String typeId, DataTypeBuilder mapper);

    String buildColumnDataType(RDBColumnMetadata columnMetaData);

    String getQuoteStart();

    String getQuoteEnd();

    String clearQuote(String string);

    boolean isColumnToUpperCase();

    Optional<SQLType> convertSqlType(Class<?> type);

    DataType convertDataType(String dataType);

    default String quote(String keyword, boolean changeCase) {
        if (keyword.startsWith(getQuoteStart()) && keyword.endsWith(getQuoteEnd())) {
            return keyword;
        }
        return StringUtils.concat(
                getQuoteStart(),
                isColumnToUpperCase() && changeCase ? keyword.toUpperCase() : keyword,
                getQuoteEnd()
        );
    }

    default String quote(String keyword) {
        return quote(keyword, true);
    }

    default String buildColumnFullName(String tableName, String columnName) {
        if (columnName.contains(".")) {
            return columnName;
        }
        if (StringUtils.isNullOrEmpty(tableName)) {
            return StringUtils.concat(getQuoteStart(), isColumnToUpperCase() ? columnName.toUpperCase() : columnName, getQuoteEnd());
        }
        return StringUtils.concat(tableName, ".", getQuoteStart(), isColumnToUpperCase() ? columnName.toUpperCase() : columnName, getQuoteEnd());
    }

    Dialect MYSQL = new MysqlDialect();
    Dialect ORACLE = new OracleDialect();
    Dialect H2 = new H2Dialect();
    Dialect MSSQL = new SqlServerDialect();
    Dialect POSTGRES = new PostgresqlDialect();
    Dialect clickhouse =new ClickhouseDialect();

}
