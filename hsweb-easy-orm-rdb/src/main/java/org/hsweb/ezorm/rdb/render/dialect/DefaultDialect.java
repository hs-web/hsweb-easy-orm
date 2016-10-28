package org.hsweb.ezorm.rdb.render.dialect;

import org.hsweb.commons.StringUtils;
import org.hsweb.ezorm.core.param.Term;
import org.hsweb.ezorm.core.param.TermType;
import org.hsweb.ezorm.rdb.executor.SqlExecutor;
import org.hsweb.ezorm.rdb.meta.RDBColumnMetaData;
import org.hsweb.ezorm.rdb.meta.parser.H2TableMetaParser;
import org.hsweb.ezorm.rdb.meta.parser.OracleTableMetaParser;
import org.hsweb.ezorm.rdb.meta.parser.TableMetaParser;
import org.hsweb.ezorm.rdb.render.Dialect;
import org.hsweb.ezorm.rdb.render.SqlAppender;

import java.sql.JDBCType;
import java.util.*;

/**
 *
 */
public abstract class DefaultDialect implements Dialect {
    protected Map<String, TermTypeMapper>   termTypeMappers       = new HashMap<>();
    protected Map<JDBCType, DataTypeMapper> dataTypeMappers       = new HashMap<>();
    protected DataTypeMapper                defaultDataTypeMapper = null;

    public DefaultDialect() {
        //默认查询条件支持
        termTypeMappers.put(TermType.eq, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(createColumnName(tableAlias, fieldMetaData.getName()), "=#{", wherePrefix, ".value}").toString());
        termTypeMappers.put(TermType.not, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(createColumnName(tableAlias, fieldMetaData.getName()), "!=#{", wherePrefix, ".value}").toString());
        termTypeMappers.put(TermType.like, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(createColumnName(tableAlias, fieldMetaData.getName()), " LIKE #{", wherePrefix, ".value}").toString());
        termTypeMappers.put(TermType.nlike, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(createColumnName(tableAlias, fieldMetaData.getName()), " NOT LIKE #{", wherePrefix, ".value}").toString());
        termTypeMappers.put(TermType.isnull, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(createColumnName(tableAlias, fieldMetaData.getName()), " IS NULL").toString());
        termTypeMappers.put(TermType.notnull, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(createColumnName(tableAlias, fieldMetaData.getName()), " IS NOT NULL").toString());
        termTypeMappers.put(TermType.gt, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(createColumnName(tableAlias, fieldMetaData.getName()), ">#{", wherePrefix, ".value}").toString());
        termTypeMappers.put(TermType.lt, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(createColumnName(tableAlias, fieldMetaData.getName()), "<#{", wherePrefix, ".value}").toString());
        termTypeMappers.put(TermType.gte, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(createColumnName(tableAlias, fieldMetaData.getName()), ">=#{", wherePrefix, ".value}").toString());
        termTypeMappers.put(TermType.lte, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(createColumnName(tableAlias, fieldMetaData.getName()), "<=#{", wherePrefix, ".value}").toString());
        termTypeMappers.put(TermType.empty, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(createColumnName(tableAlias, fieldMetaData.getName()), "=''").toString());
        termTypeMappers.put(TermType.nempty, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(createColumnName(tableAlias, fieldMetaData.getName()), "!=''").toString());
        termTypeMappers.put(TermType.func, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(term.getValue()).toString());
        termTypeMappers.put(TermType.btw, (wherePrefix, term, fieldMetaData, tableAlias) -> {
            SqlAppender sqlAppender = new SqlAppender();
            List<Object> objects = param2list(term.getValue());
            if (objects.size() == 1)
                objects.add(objects.get(0));
            term.setValue(objects);
            sqlAppender.add(createColumnName(tableAlias, fieldMetaData.getName()), " ").addSpc("BETWEEN")
                    .add("#{", wherePrefix, ".value[0]}")
                    .add(" AND ", "#{", wherePrefix, ".value[1]}");
            return sqlAppender.toString();
        });
        termTypeMappers.put(TermType.nbtw, (wherePrefix, term, fieldMetaData, tableAlias) ->
        {
            SqlAppender sqlAppender = new SqlAppender();
            List<Object> objects = param2list(term.getValue());
            if (objects.size() == 1)
                objects.add(objects.get(0));
            term.setValue(objects);
            sqlAppender.add(createColumnName(tableAlias, fieldMetaData.getName()), " ").addSpc("NOT BETWEEN")
                    .add("#{", wherePrefix, ".value[0]}")
                    .add(" AND ", "#{", wherePrefix, ".value[1]}");
            return sqlAppender.toString();
        });
        termTypeMappers.put(TermType.in, (wherePrefix, term, fieldMetaData, tableAlias) -> {
            List<Object> values = param2list(term.getValue());
            term.setValue(values);
            SqlAppender appender = new SqlAppender();
            appender.add(tableAlias, ".").addSpc(fieldMetaData.getName()).add("IN(");
            for (int i = 0; i < values.size(); i++) {
                appender.add("#{", wherePrefix, ".value[", i, "]}", ",");
            }
            appender.removeLast();
            appender.add(")");
            return appender.toString();
        });
        termTypeMappers.put(TermType.nin, (wherePrefix, term, fieldMetaData, tableAlias) -> {
            List<Object> values = param2list(term.getValue());
            term.setValue(values);
            SqlAppender appender = new SqlAppender();
            appender.add(tableAlias, ".").addSpc(fieldMetaData.getName()).add("NOT IN(");
            for (int i = 0; i < values.size(); i++) {
                appender.add("#{", wherePrefix, ".value[", i, "]}", ",");
            }
            appender.removeLast();
            appender.add(")");
            return appender.toString();
        });
    }

    @Override
    public String wrapperWhere(String wherePrefix, Term term, RDBColumnMetaData RDBColumnMetaData, String tableAlias) {
        TermTypeMapper mapper = termTypeMappers.get(term.getTermType());
        if (mapper == null) mapper = termTypeMappers.get(TermType.eq);
        return mapper.accept(wherePrefix, term, RDBColumnMetaData, tableAlias);
    }

    protected List<Object> param2list(Object value) {
        if (value == null) return new ArrayList<>();
        if (value instanceof List) return ((List) value);
        if (value instanceof Collection) return new ArrayList<>(((Collection) value));

        if (!(value instanceof Collection)) {
            if (value instanceof String) {
                String[] arr = ((String) value).split("[, ;]");
                Object[] objArr = new Object[arr.length];
                for (int i = 0; i < arr.length; i++) {
                    String str = arr[i];
                    Object val = str;
                    if (StringUtils.isInt(str))
                        val = StringUtils.toInt(str);
                    else if (StringUtils.isDouble(str))
                        val = StringUtils.toDouble(str);
                    objArr[i] = val;
                }
                return Arrays.asList(objArr);
            } else if (value.getClass().isArray()) {
                return Arrays.asList(((Object[]) value));
            } else {
                return new ArrayList<>(Arrays.asList(value));
            }
        }
        return new ArrayList<>();
    }

    @Override
    public void setTermTypeMapper(String termType, TermTypeMapper mapper) {
        termType = termType.toLowerCase();
        termTypeMappers.put(termType, mapper);
    }

    @Override
    public void setDataTypeMapper(JDBCType jdbcType, DataTypeMapper mapper) {
        dataTypeMappers.put(jdbcType, mapper);
    }

    @Override
    public void setColumnMapper(String columnType, ColumnMapper mapper) {
        // TODO: 16-10-28
    }

    @Override
    public String buildDataType(RDBColumnMetaData columnMetaData) {
        DataTypeMapper mapper = dataTypeMappers.get(columnMetaData.getJdbcType());
        if (null == mapper) mapper = defaultDataTypeMapper;
        return mapper.getDataType(columnMetaData);
    }

    @Override
    public TableMetaParser getDefaultParser(SqlExecutor sqlExecutor) {
        return new OracleTableMetaParser(sqlExecutor);
    }

}
