package org.hsweb.ezorm.rdb.render;

import org.hsweb.commons.StringUtils;
import org.hsweb.ezorm.rdb.executor.SqlExecutor;
import org.hsweb.ezorm.rdb.meta.RDBColumnMetaData;
import org.hsweb.ezorm.core.param.Term;
import org.hsweb.ezorm.rdb.meta.parser.TableMetaParser;
import org.hsweb.ezorm.rdb.render.dialect.DefaultDialect;

import java.sql.JDBCType;

public interface Dialect {
    interface TermTypeMapper {
        String accept(String wherePrefix, Term term, RDBColumnMetaData RDBColumnMetaData, String tableAlias);
    }

    interface DataTypeMapper {
        String getDataType(RDBColumnMetaData columnMetaData);
    }

    interface ColumnMapper {
        String getColumn(RDBColumnMetaData columnMetaData);
    }

    void setTermTypeMapper(String termType, TermTypeMapper mapper);

    void setDataTypeMapper(JDBCType jdbcType, DataTypeMapper mapper);

    void setColumnMapper(String columnType, ColumnMapper mapper);

    String getQuoteStart();

    String getQuoteEnd();

    String wrapperWhere(String wherePrefix, Term term, RDBColumnMetaData RDBColumnMetaData, String tableAlias);

    String buildDataType(RDBColumnMetaData columnMetaData);

    String doPaging(String sql, int pageIndex, int pageSize);

    boolean columnToUpperCase();

    default String createColumnName(String tableName, String columnName) {
        return StringUtils.concat(tableName, ".", getQuoteStart(), columnToUpperCase() ? columnName.toUpperCase() : columnName, getQuoteEnd());
    }

    TableMetaParser getDefaultParser(SqlExecutor sqlExecutor);
}
