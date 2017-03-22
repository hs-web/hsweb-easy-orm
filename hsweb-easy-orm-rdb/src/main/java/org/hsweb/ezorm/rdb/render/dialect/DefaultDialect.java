package org.hsweb.ezorm.rdb.render.dialect;

import org.hsweb.commons.StringUtils;
import org.hsweb.ezorm.core.param.SqlTerm;
import org.hsweb.ezorm.core.param.Term;
import org.hsweb.ezorm.core.param.TermType;
import org.hsweb.ezorm.rdb.executor.SqlExecutor;
import org.hsweb.ezorm.rdb.meta.RDBColumnMetaData;
import org.hsweb.ezorm.rdb.meta.parser.OracleTableMetaParser;
import org.hsweb.ezorm.rdb.meta.parser.TableMetaParser;
import org.hsweb.ezorm.rdb.render.SqlAppender;

import java.sql.JDBCType;
import java.util.*;

/**
 *
 */
public abstract class DefaultDialect implements Dialect {
    protected Map<String, TermTypeMapper> termTypeMappers       = new HashMap<>();
    protected Map<String, DataTypeMapper> dataTypeMappers       = new HashMap<>();
    protected DataTypeMapper              defaultDataTypeMapper = null;

    static final List<JDBCType> numberJdbcType = Arrays.asList(JDBCType.NUMERIC, JDBCType.INTEGER, JDBCType.BIGINT, JDBCType.TINYINT, JDBCType.DOUBLE, JDBCType.FLOAT);

    public DefaultDialect() {
        //默认查询条件支持
        termTypeMappers.put(TermType.eq, (wherePrefix, term, column, tableAlias) ->
                new SqlAppender().add(buildColumnName(tableAlias, column.getName()), "=#{", wherePrefix, ".value}"));
        termTypeMappers.put(TermType.not, (wherePrefix, term, column, tableAlias) ->
                new SqlAppender().add(buildColumnName(tableAlias, column.getName()), "!=#{", wherePrefix, ".value}"));
        termTypeMappers.put(TermType.like, (wherePrefix, term, column, tableAlias) ->
                new SqlAppender().add(buildColumnName(tableAlias, column.getName()), " LIKE #{", wherePrefix, ".value}"));
        termTypeMappers.put(TermType.nlike, (wherePrefix, term, column, tableAlias) ->
                new SqlAppender().add(buildColumnName(tableAlias, column.getName()), " NOT LIKE #{", wherePrefix, ".value}"));
        termTypeMappers.put(TermType.isnull, (wherePrefix, term, column, tableAlias) ->
                new SqlAppender().add(buildColumnName(tableAlias, column.getName()), " IS NULL"));
        termTypeMappers.put(TermType.notnull, (wherePrefix, term, column, tableAlias) ->
                new SqlAppender().add(buildColumnName(tableAlias, column.getName()), " IS NOT NULL"));
        termTypeMappers.put(TermType.gt, (wherePrefix, term, column, tableAlias) ->
                new SqlAppender().add(buildColumnName(tableAlias, column.getName()), ">#{", wherePrefix, ".value}"));
        termTypeMappers.put(TermType.lt, (wherePrefix, term, column, tableAlias) ->
                new SqlAppender().add(buildColumnName(tableAlias, column.getName()), "<#{", wherePrefix, ".value}"));
        termTypeMappers.put(TermType.gte, (wherePrefix, term, column, tableAlias) ->
                new SqlAppender().add(buildColumnName(tableAlias, column.getName()), ">=#{", wherePrefix, ".value}"));
        termTypeMappers.put(TermType.lte, (wherePrefix, term, column, tableAlias) ->
                new SqlAppender().add(buildColumnName(tableAlias, column.getName()), "<=#{", wherePrefix, ".value}"));
        termTypeMappers.put(TermType.empty, (wherePrefix, term, column, tableAlias) ->
                new SqlAppender().add(buildColumnName(tableAlias, column.getName()), "=''"));
        termTypeMappers.put(TermType.nempty, (wherePrefix, term, column, tableAlias) ->
                new SqlAppender().add(buildColumnName(tableAlias, column.getName()), "!=''"));
        //TermType.func is not support
//        termTypeMappers.put(TermType.func, (wherePrefix, term, column, tableAlias) ->
//                new SqlAppender().add(term.getValue()));
        termTypeMappers.put(TermType.btw, (wherePrefix, term, column, tableAlias) -> {
            SqlAppender sqlAppender = new SqlAppender();
            List<Object> objects = param2list(term.getValue(), column);
            if (objects.size() == 1)
                objects.add(objects.get(0));
            term.setValue(objects);
            sqlAppender.add(buildColumnName(tableAlias, column.getName()), " ").addSpc("BETWEEN")
                    .add("#{", wherePrefix, ".value[0]}")
                    .add(" AND ", "#{", wherePrefix, ".value[1]}");
            return sqlAppender;
        });
        termTypeMappers.put(TermType.nbtw, (wherePrefix, term, column, tableAlias) ->
        {
            SqlAppender sqlAppender = new SqlAppender();
            List<Object> objects = param2list(term.getValue(), column);
            if (objects.size() == 1)
                objects.add(objects.get(0));
            term.setValue(objects);
            sqlAppender.add(buildColumnName(tableAlias, column.getName()), " ").addSpc("NOT BETWEEN")
                    .add("#{", wherePrefix, ".value[0]}")
                    .add(" AND ", "#{", wherePrefix, ".value[1]}");
            return sqlAppender;
        });
        termTypeMappers.put(TermType.in, (wherePrefix, term, column, tableAlias) -> {
            List<Object> values = param2list(term.getValue(), column);
            term.setValue(values);
            SqlAppender appender = new SqlAppender();
            appender.add(tableAlias, ".").addSpc(column.getName()).add("IN(");
            for (int i = 0; i < values.size(); i++) {
                appender.add("#{", wherePrefix, ".value[", i, "]}", ",");
            }
            appender.removeLast();
            appender.add(")");
            return appender;
        });
        termTypeMappers.put(TermType.nin, (wherePrefix, term, column, tableAlias) -> {
            List<Object> values = param2list(term.getValue(), column);
            term.setValue(values);
            SqlAppender appender = new SqlAppender();
            appender.add(tableAlias, ".").addSpc(column.getName()).add("NOT IN(");
            for (int i = 0; i < values.size(); i++) {
                appender.add("#{", wherePrefix, ".value[", i, "]}", ",");
            }
            appender.removeLast();
            appender.add(")");
            return appender;
        });
    }

    @Override
    public SqlAppender buildCondition(String wherePrefix, Term term, RDBColumnMetaData RDBColumnMetaData, String tableAlias) {
        if (term instanceof SqlTerm) {
            SqlTerm sqlTerm = ((SqlTerm) term);
            TermTypeMapper mapper = TermTypeMapper.sql(sqlTerm.getSql(), sqlTerm.getParam());
            return mapper.accept(wherePrefix, sqlTerm, RDBColumnMetaData, tableAlias);
        }
        if (term.getValue() instanceof TermTypeMapper) {
            return ((TermTypeMapper) term.getValue()).accept(wherePrefix, term, RDBColumnMetaData, tableAlias);
        }
        TermTypeMapper mapper = termTypeMappers.get(term.getTermType());
        if (mapper == null) mapper = termTypeMappers.get(TermType.eq);
        return mapper.accept(wherePrefix, term, RDBColumnMetaData, tableAlias);
    }

    @SuppressWarnings("unchecked")
    protected List<Object> param2list(Object value, RDBColumnMetaData columnMetaData) {
        if (value == null) return new ArrayList<>();
        if (value instanceof List) return (List) value;
        if (value instanceof Collection) return new ArrayList<>(((Collection) value));
        if (value instanceof String) {
            String[] arr = ((String) value).split("[, ;]");
            Object[] objArr = new Object[arr.length];
            for (int i = 0; i < arr.length; i++) {
                String str = arr[i];
                Object val = str;
                //数字类型
                if (numberJdbcType.contains(columnMetaData.getJdbcType())) {
                    if (StringUtils.isInt(str))
                        val = StringUtils.toInt(str);
                    else if (StringUtils.isDouble(str))
                        val = StringUtils.toDouble(str);
                }
                objArr[i] = val;
            }
            return Arrays.asList(objArr);
        } else if (value.getClass().isArray()) {
            return Arrays.asList(((Object[]) value));
        } else {
            return new ArrayList<>(Arrays.asList(value));
        }
    }

    @Override
    public void setTermTypeMapper(String termType, TermTypeMapper mapper) {
        termType = termType.toLowerCase();
        termTypeMappers.put(termType, mapper);
    }

    @Override
    public void setDataTypeMapper(JDBCType jdbcType, DataTypeMapper mapper) {
        dataTypeMappers.put(jdbcType.getName(), mapper);
    }

    @Override
    public void setColumnMapper(String columnType, ColumnMapper mapper) {
        // TODO: 16-10-28
    }

    @Override
    public String buildDataType(RDBColumnMetaData columnMetaData) {
        if (columnMetaData.getJdbcType() == null) return null;
        DataTypeMapper mapper = dataTypeMappers.get(columnMetaData.getJdbcType().getName());
        if (null == mapper) mapper = defaultDataTypeMapper;
        return mapper.getDataType(columnMetaData);
    }

    @Override
    public TableMetaParser getDefaultParser(SqlExecutor sqlExecutor) {
        return new OracleTableMetaParser(sqlExecutor);
    }

}
