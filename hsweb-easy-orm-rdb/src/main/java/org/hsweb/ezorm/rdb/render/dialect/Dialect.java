package org.hsweb.ezorm.rdb.render.dialect;

import org.hsweb.commons.StringUtils;
import org.hsweb.ezorm.core.param.Term;
import org.hsweb.ezorm.rdb.executor.SqlExecutor;
import org.hsweb.ezorm.rdb.meta.RDBColumnMetaData;
import org.hsweb.ezorm.rdb.meta.parser.TableMetaParser;
import org.hsweb.ezorm.rdb.render.SqlAppender;

import java.sql.JDBCType;
import java.util.regex.Matcher;

import static org.hsweb.ezorm.rdb.executor.AbstractJdbcSqlExecutor.APPEND_PATTERN;
import static org.hsweb.ezorm.rdb.executor.AbstractJdbcSqlExecutor.PREPARED_PATTERN;

public interface Dialect {
    interface TermTypeMapper {
        SqlAppender accept(String wherePrefix, Term term, RDBColumnMetaData column, String tableAlias);

        static TermTypeMapper sql(String sql) {
            return (wherePrefix, term, column, tableAlias) -> new SqlAppender(sql);
        }

        static TermTypeMapper sql(String sql, Object param) {

            return (wherePrefix, term, column, tableAlias) -> {
                Object finalParam = param;
                String template = sql;
                //?方式预编译
                if (template.contains("?")) {
                    int index = 0;
                    while (template.contains("?")) {
                        template = template.replaceFirst("\\?", "#\\{[" + index++ + "]}");
                    }
                } else if (finalParam instanceof Object[]) {
                    Object[] array = ((Object[]) finalParam);
                    if (array.length == 1) {
                        finalParam = array[0];
                    }
                }
                Matcher prepared_matcher = PREPARED_PATTERN.matcher(template);
                Matcher append_matcher = APPEND_PATTERN.matcher(template);
                term.setValue(finalParam);
                while (append_matcher.find()) {
                    String group = append_matcher.group();
                    String reg = StringUtils.concat("\\$\\{", group.replace("$", "\\$").replace("[", "\\[").replace("]", "\\]"), "}");
                    String target = StringUtils.concat("\\$\\{", wherePrefix, group.startsWith("[") ? ".value" : ".value.", group, "}");
                    template = template.replaceFirst(reg, target);

                }
                while (prepared_matcher.find()) {
                    String group = prepared_matcher.group();
                    template = template.replaceFirst(StringUtils.concat("#\\{", group.replace("$", "\\$").replace("[", "\\[").replace("]", "\\]"), "}"),
                            StringUtils.concat("#\\{", wherePrefix, group.startsWith("[") ? ".value" : ".value.", group, "}"));
                }
                return new SqlAppender(template);
            };
        }

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

    SqlAppender buildCondition(String wherePrefix, Term term, RDBColumnMetaData RDBColumnMetaData, String tableAlias);

    String buildDataType(RDBColumnMetaData columnMetaData);

    String doPaging(String sql, int pageIndex, int pageSize);

    boolean columnToUpperCase();

    default String buildColumnName(String tableName, String columnName) {
        if (StringUtils.isNullOrEmpty(tableName)) {
            return StringUtils.concat(getQuoteStart(), columnToUpperCase() ? columnName.toUpperCase() : columnName, getQuoteEnd());
        }
        return StringUtils.concat(tableName, ".", getQuoteStart(), columnToUpperCase() ? columnName.toUpperCase() : columnName, getQuoteEnd());
    }

    TableMetaParser getDefaultParser(SqlExecutor sqlExecutor);

    Dialect MYSQL  = new MysqlDialect();
    Dialect ORACLE = new OracleDialect();
    Dialect H2     = new H2Dialect();

    Dialect MSSQL = new MSSQLDialect();

}
