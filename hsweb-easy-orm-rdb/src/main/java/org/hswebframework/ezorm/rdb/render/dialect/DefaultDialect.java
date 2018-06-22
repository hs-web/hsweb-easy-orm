package org.hswebframework.ezorm.rdb.render.dialect;

import org.hswebframework.ezorm.core.param.ClassFieldTerm;
import org.hswebframework.ezorm.rdb.render.dialect.function.SqlFunction;
import org.hswebframework.ezorm.rdb.render.dialect.term.BoostTermTypeMapper;
import org.hswebframework.ezorm.core.param.SqlTerm;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;
import org.hswebframework.ezorm.rdb.executor.SqlExecutor;
import org.hswebframework.ezorm.rdb.meta.RDBColumnMetaData;
import org.hswebframework.ezorm.rdb.meta.parser.OracleTableMetaParser;
import org.hswebframework.ezorm.rdb.meta.parser.TableMetaParser;
import org.hswebframework.ezorm.rdb.render.SqlAppender;

import java.sql.JDBCType;
import java.util.*;

/**
 *
 */
public abstract class DefaultDialect implements Dialect {
    protected Map<String, TermTypeMapper> termTypeMappers       = new HashMap<>();
    protected Map<String, DataTypeMapper> dataTypeMappers       = new HashMap<>();
    protected Map<String, SqlFunction>    functions             = new HashMap<>();
    protected DataTypeMapper              defaultDataTypeMapper = null;

    static final List<JDBCType> numberJdbcType = Arrays.asList(JDBCType.NUMERIC, JDBCType.INTEGER, JDBCType.BIGINT, JDBCType.TINYINT, JDBCType.DOUBLE, JDBCType.FLOAT);

    public DefaultDialect() {
        //默认查询条件支持
        termTypeMappers.put(TermType.eq, BoostTermTypeMapper.notSupportArray((wherePrefix, term, column, tableAlias) ->
                new SqlAppender().add(
                        buildColumnName(tableAlias, column.getName()),
                        "=#{", wherePrefix,
                        (term instanceof ClassFieldTerm) ? "" : ".value}")));

        termTypeMappers.put(TermType.not, BoostTermTypeMapper.notSupportArray((wherePrefix, term, column, tableAlias) ->
                new SqlAppender().add(buildColumnName(tableAlias, column.getName()), "!=#{", wherePrefix, (term instanceof ClassFieldTerm) ? "" : ".value}")));

        termTypeMappers.put(TermType.like, BoostTermTypeMapper.notSupportArray((wherePrefix, term, column, tableAlias) ->
                new SqlAppender().add(buildColumnName(tableAlias, column.getName()), " LIKE #{", wherePrefix, (term instanceof ClassFieldTerm) ? "" : ".value}")));

        termTypeMappers.put(TermType.nlike, (wherePrefix, term, column, tableAlias) ->
                new SqlAppender().add(buildColumnName(tableAlias, column.getName()), " NOT LIKE #{", wherePrefix, (term instanceof ClassFieldTerm) ? "" : ".value}"));
        termTypeMappers.put(TermType.isnull, (wherePrefix, term, column, tableAlias) ->
                new SqlAppender().add(buildColumnName(tableAlias, column.getName()), " IS NULL"));
        termTypeMappers.put(TermType.notnull, (wherePrefix, term, column, tableAlias) ->
                new SqlAppender().add(buildColumnName(tableAlias, column.getName()), " IS NOT NULL"));
        termTypeMappers.put(TermType.gt, BoostTermTypeMapper.notSupportArray((wherePrefix, term, column, tableAlias) ->
                new SqlAppender().add(buildColumnName(tableAlias, column.getName()), ">#{", wherePrefix, (term instanceof ClassFieldTerm) ? "" : ".value}")));
        termTypeMappers.put(TermType.lt, BoostTermTypeMapper.notSupportArray((wherePrefix, term, column, tableAlias) ->
                new SqlAppender().add(buildColumnName(tableAlias, column.getName()), "<#{", wherePrefix, (term instanceof ClassFieldTerm) ? "" : ".value}")));
        termTypeMappers.put(TermType.gte, BoostTermTypeMapper.notSupportArray((wherePrefix, term, column, tableAlias) ->
                new SqlAppender().add(buildColumnName(tableAlias, column.getName()), ">=#{", wherePrefix, (term instanceof ClassFieldTerm) ? "" : ".value}")));
        termTypeMappers.put(TermType.lte, BoostTermTypeMapper.notSupportArray((wherePrefix, term, column, tableAlias) ->
                new SqlAppender().add(buildColumnName(tableAlias, column.getName()), "<=#{", wherePrefix, (term instanceof ClassFieldTerm) ? "" : ".value}")));
        termTypeMappers.put(TermType.empty, BoostTermTypeMapper.notSupportArray((wherePrefix, term, column, tableAlias) ->
                new SqlAppender().add(buildColumnName(tableAlias, column.getName()), "=''")));
        termTypeMappers.put(TermType.nempty, BoostTermTypeMapper.notSupportArray((wherePrefix, term, column, tableAlias) ->
                new SqlAppender().add(buildColumnName(tableAlias, column.getName()), "!=''")));

        termTypeMappers.put(TermType.btw, BoostTermTypeMapper.supportArray((wherePrefix, term, column, tableAlias) -> {
            SqlAppender sqlAppender = new SqlAppender();
            List<Object> objects = param2list(term.getValue());
            if (objects.size() == 1)
                objects.add(objects.get(0));
            term.setValue(objects);
            sqlAppender.add(buildColumnName(tableAlias, column.getName()), " ").addSpc("BETWEEN")
                    .add("#{", wherePrefix, ".value[0]}")
                    .add(" AND ", "#{", wherePrefix, ".value[1]}");
            return sqlAppender;
        }));
        termTypeMappers.put(TermType.nbtw, BoostTermTypeMapper.supportArray((wherePrefix, term, column, tableAlias) ->
        {
            SqlAppender sqlAppender = new SqlAppender();
            List<Object> objects = param2list(term.getValue());
            if (objects.size() == 1)
                objects.add(objects.get(0));
            term.setValue(objects);
            sqlAppender.add(buildColumnName(tableAlias, column.getName()), " ").addSpc("NOT BETWEEN")
                    .add("#{", wherePrefix, ".value[0]}")
                    .add(" AND ", "#{", wherePrefix, ".value[1]}");
            return sqlAppender;
        }));
        termTypeMappers.put(TermType.in, BoostTermTypeMapper.supportArray((wherePrefix, term, column, tableAlias) -> {
            List<Object> values = param2list(term.getValue());
            term.setValue(values);
            SqlAppender appender = new SqlAppender();
            appender.add(tableAlias, ".").addSpc(column.getName()).add("IN(");
            for (int i = 0; i < values.size(); i++) {
                appender.add("#{", wherePrefix, ".value[", i, "]}", ",");
            }
            appender.removeLast();
            appender.add(")");
            return appender;
        }));
        termTypeMappers.put(TermType.nin, BoostTermTypeMapper.supportArray((wherePrefix, term, column, tableAlias) -> {
            List<Object> values = param2list(term.getValue());
            term.setValue(values);
            SqlAppender appender = new SqlAppender();
            appender.add(tableAlias, ".").addSpc(column.getName()).add("NOT IN(");
            for (int i = 0; i < values.size(); i++) {
                appender.add("#{", wherePrefix, ".value[", i, "]}", ",");
            }
            appender.removeLast();
            appender.add(")");
            return appender;
        }));
    }

    @Override
    public SqlFunction getFunction(String name) {
        return functions.get(name);
    }

    @Override
    public SqlFunction installFunction(String name, SqlFunction function) {
        return functions.put(name, function);
    }

    @Override
    public SqlAppender buildCondition(String wherePrefix, Term term, RDBColumnMetaData column, String tableAlias) {
        if (term instanceof SqlTerm) {
            SqlTerm sqlTerm = ((SqlTerm) term);
            String sql = sqlTerm.getSql();
            if (sql == null) sql = sqlTerm.getColumn();
            if (sql == null) return new SqlAppender();
            TermTypeMapper mapper = TermTypeMapper.sql(sqlTerm.getSql(), sqlTerm.getParam());
            return mapper.accept(wherePrefix, sqlTerm, column, tableAlias);
        }
        if (term.getValue() instanceof TermTypeMapper) {
            return ((TermTypeMapper) term.getValue()).accept(wherePrefix, term, column, tableAlias);
        }
        TermTypeMapper mapper = termTypeMappers.get(term.getTermType());
        if (mapper == null) mapper = termTypeMappers.get(TermType.eq);
        return mapper.accept(wherePrefix, term, column, tableAlias);
    }

    @SuppressWarnings("unchecked")
    protected List<Object> param2list(Object value) {
        if (value == null) return new ArrayList<>();
        if (value instanceof List) return (List) value;
        if (value instanceof Collection) return new ArrayList<>(((Collection) value));
        if (value.getClass().isArray()) {
            return new ArrayList<>(Arrays.asList(((Object[]) value)));
        } else {
            return new ArrayList<>(Collections.singletonList(value));
        }
    }

    @Override
    public void setTermTypeMapper(String termType, TermTypeMapper mapper) {
        termTypeMappers.put(termType.toLowerCase(), mapper);
    }

    @Override
    public void setDataTypeMapper(JDBCType jdbcType, DataTypeMapper mapper) {
        dataTypeMappers.put(jdbcType.getName(), mapper);
    }

    @Override
    public String buildDataType(RDBColumnMetaData columnMetaData) {
        if (columnMetaData.getJdbcType() == null) return null;
        DataTypeMapper mapper = dataTypeMappers.get(columnMetaData.getJdbcType().getName());
        if (null == mapper) mapper = defaultDataTypeMapper;
        return mapper.getDataType(columnMetaData);
    }

}
