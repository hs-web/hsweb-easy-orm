package org.hswebframework.ezorm.rdb.render.dialect;

import org.hswebframework.ezorm.rdb.executor.AbstractJdbcSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlExecutor;
import org.hswebframework.ezorm.rdb.meta.RDBColumnMetaData;
import org.hswebframework.ezorm.rdb.meta.parser.TableMetaParser;
import org.hswebframework.ezorm.rdb.render.SqlAppender;
import org.hswebframework.utils.StringUtils;
import org.hswebframework.ezorm.core.param.Term;

import java.sql.JDBCType;
import java.util.regex.Matcher;

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
                Matcher prepared_matcher = AbstractJdbcSqlExecutor.PREPARED_PATTERN.matcher(template);
                Matcher append_matcher = AbstractJdbcSqlExecutor.APPEND_PATTERN.matcher(template);
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

    SqlAppender buildCondition(String wherePrefix, Term term, RDBColumnMetaData column, String tableAlias);

    String buildDataType(RDBColumnMetaData columnMetaData);

    String doPaging(String sql, int pageIndex, int pageSize);

    boolean columnToUpperCase();

    default String buildColumnName(String tableName, String columnName) {
        if (columnName.contains(".")) return columnName;
        if (StringUtils.isNullOrEmpty(tableName)) {
            return StringUtils.concat(getQuoteStart(), columnToUpperCase() ? columnName.toUpperCase() : columnName, getQuoteEnd());
        }
        return StringUtils.concat(tableName, ".", getQuoteStart(), columnToUpperCase() ? columnName.toUpperCase() : columnName, getQuoteEnd());
    }

    TableMetaParser getDefaultParser(SqlExecutor sqlExecutor);

    Dialect MYSQL  = new MysqlDialect();
    Dialect ORACLE = new OracleDialect();
    Dialect H2     = new H2Dialect();
    Dialect MSSQL  = new MSSQLDialect();

}
