package org.hsweb.ezorm.render.dialect;

import org.hsweb.commons.StringUtils;
import org.hsweb.ezorm.meta.FieldMetaData;
import org.hsweb.ezorm.param.Term;
import org.hsweb.ezorm.param.TermType;
import org.hsweb.ezorm.render.Dialect;
import org.hsweb.ezorm.render.SqlAppender;

import java.util.*;

/**
 * Created by zhouhao on 16-6-4.
 */
public abstract class DefaultDialect implements Dialect {

    protected Map<String, Mapper> termTypeMappers = new HashMap<>();

    public DefaultDialect() {
        termTypeMappers.put(TermType.eq, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(tableAlias, ".", fieldMetaData.getName(), "=#{", wherePrefix, ".value}").toString());
        termTypeMappers.put(TermType.not, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(tableAlias, ".", fieldMetaData.getName(), "!=#{", wherePrefix, ".value}").toString());
        termTypeMappers.put(TermType.like, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(tableAlias, ".", fieldMetaData.getName(), " LIKE #{", wherePrefix, ".value}").toString());
        termTypeMappers.put(TermType.notlike, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(tableAlias, ".", fieldMetaData.getName(), " NOT LIKE #{", wherePrefix, ".value}").toString());
        termTypeMappers.put(TermType.isnull, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(tableAlias, ".", fieldMetaData.getName(), " IS NULL").toString());
        termTypeMappers.put(TermType.notnull, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(tableAlias, ".", fieldMetaData.getName(), " IS NOT NULL").toString());

        termTypeMappers.put(TermType.gt, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(tableAlias, ".", fieldMetaData.getName(), ">#{", wherePrefix, ".value}").toString());

        termTypeMappers.put(TermType.lt, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(tableAlias, ".", fieldMetaData.getName(), "<#{", wherePrefix, ".value}").toString());

        termTypeMappers.put(TermType.gtoreq, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(tableAlias, ".", fieldMetaData.getName(), ">=#{", wherePrefix, ".value}").toString());

        termTypeMappers.put(TermType.ltoreq, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(tableAlias, ".", fieldMetaData.getName(), "<=#{", wherePrefix, ".value}").toString());

        termTypeMappers.put(TermType.empty, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(tableAlias, ".", fieldMetaData.getName(), "=''").toString());
        termTypeMappers.put(TermType.notempty, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(tableAlias, ".", fieldMetaData.getName(), "!=''").toString());

        termTypeMappers.put(TermType.func, (wherePrefix, term, fieldMetaData, tableAlias) ->
                new SqlAppender().add(term.getValue()).toString());

        termTypeMappers.put(TermType.btw, (wherePrefix, term, fieldMetaData, tableAlias) -> {
            SqlAppender sqlAppender = new SqlAppender();
            List<Object> objects = param2list(term.getValue());
            if (objects.size() == 1)
                objects.add(objects.get(0));
            term.setValue(objects);
            sqlAppender.add(tableAlias, ".", fieldMetaData.getName(), " ").addSpc("BETWEEN")
                    .add(" #{", wherePrefix, ".value[0]}")
                    .add(" AND ", "#{", wherePrefix, ".value[1]}");
            return sqlAppender.toString();
        });
        termTypeMappers.put(TermType.notbtw, (wherePrefix, term, fieldMetaData, tableAlias) ->
        {
            SqlAppender sqlAppender = new SqlAppender();
            List<Object> objects = param2list(term.getValue());
            if (objects.size() == 1)
                objects.add(objects.get(0));
            term.setValue(objects);
            sqlAppender.add(tableAlias, ".", fieldMetaData.getName(), " ").addSpc(" NOT BETWEEN")
                    .add(" #{", wherePrefix, ".value[0]}")
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
        termTypeMappers.put(TermType.notin, (wherePrefix, term, fieldMetaData, tableAlias) -> {
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
    public String wrapperWhere(String wherePrefix, Term term, FieldMetaData fieldMetaData, String tableAlias) {
        Mapper mapper = termTypeMappers.get(term.getTermType());
        if (mapper == null) mapper = termTypeMappers.get(TermType.eq);
        return mapper.accept(wherePrefix, term, fieldMetaData, tableAlias);
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
    public void setTermTypeMapper(String termType, Mapper mapper) {
        termType = termType.toLowerCase();
        termTypeMappers.put(termType, mapper);
    }
}
