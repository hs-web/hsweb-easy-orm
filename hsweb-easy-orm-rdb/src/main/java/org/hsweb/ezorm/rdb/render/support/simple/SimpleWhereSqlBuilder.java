package org.hsweb.ezorm.rdb.render.support.simple;

import org.hswebframework.utils.StringUtils;
import org.hsweb.ezorm.core.param.SqlTerm;
import org.hsweb.ezorm.core.param.Term;
import org.hsweb.ezorm.core.param.TermType;
import org.hsweb.ezorm.rdb.meta.Correlation;
import org.hsweb.ezorm.rdb.meta.RDBColumnMetaData;
import org.hsweb.ezorm.rdb.meta.RDBTableMetaData;
import org.hsweb.ezorm.rdb.render.SqlAppender;
import org.hsweb.ezorm.rdb.render.dialect.Dialect;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public abstract class SimpleWhereSqlBuilder {

    private Set<String> doNotTransformationValueTermType = Stream.of(
            TermType.isnull,
            TermType.notnull,
            TermType.empty,
            TermType.nempty
    ).collect(Collectors.toSet());

    protected String getTableAlias(RDBTableMetaData metaData, String field) {
        if (field.contains("."))
            field = field.split("[.]")[0];
        else return metaData.getAlias();
        Correlation correlation = metaData.getCorrelation(field);
        if (correlation != null) return correlation.getAlias();
        return metaData.getAlias();
    }

    public void buildWhere(RDBTableMetaData metaData, String prefix,
                           List<Term> terms, SqlAppender appender,
                           Set<String> needSelectTable) {
        if (terms == null || terms.isEmpty()) return;
        int index = -1;
        String prefixTmp = StringUtils.concat(prefix, StringUtils.isNullOrEmpty(prefix) ? "" : ".");
        for (Term term : terms) {
            index++;
            boolean nullTerm = StringUtils.isNullOrEmpty(term.getColumn());
            RDBColumnMetaData column = metaData.findColumn(term.getColumn());
            if (!(term instanceof SqlTerm)) {
                //不是空条件 也不是可选字段
                if (!nullTerm && column == null) continue;
                //不是空条件，值为空
                if (!nullTerm && StringUtils.isNullOrEmpty(term.getValue())) continue;
                //是空条件，但是无嵌套
                if (nullTerm && term.getTerms().isEmpty()) continue;
            } else {
                if (StringUtils.isNullOrEmpty(((SqlTerm) term).getSql())) continue;
            }
            String tableAlias = null;
            if (column != null) {
                tableAlias = getTableAlias(metaData, term.getColumn());
                needSelectTable.add(tableAlias);
                //部分termType不需要转换
                if (!doNotTransformationValueTermType.contains(term.getTermType()))
                    //转换参数的值
                    term.setValue(transformationValue(column, term.getValue()));
            }
            //用于sql预编译的参数名
            prefix = StringUtils.concat(prefixTmp, "terms[", index, "]");
            //添加类型，and 或者 or
            appender.add(StringUtils.concat(" ", term.getType().toString().toUpperCase(), " "));
            if (!term.getTerms().isEmpty()) {
                //构建嵌套的条件
                SqlAppender nest = new SqlAppender();
                buildWhere(metaData, prefix, term.getTerms(), nest, needSelectTable);
                //如果嵌套结果为空,
                if (nest.isEmpty()) {
                    appender.removeLast();//删除最后一个（and 或者 or）
                    continue;
                }
                if (nullTerm) {
                    //删除 第一个（and 或者 or）
                    nest.removeFirst();
                }
                appender.add("(");
                if (!nullTerm)
                    appender.add(getDialect().buildCondition(prefix, term, column, tableAlias));
                appender.addAll(nest);
                appender.add(")");
            } else {
                if (!nullTerm)
                    appender.add(getDialect().buildCondition(prefix, term, column, tableAlias));
            }
        }
    }

    protected Object transformationValue(RDBColumnMetaData column, Object value) {
        if (value != null && column.getValueConverter() != null) {
            value = column.getValueConverter().getData(value);
        }
        if (value != null && column.getOptionConverter() != null) {
            Object tmp = column.getOptionConverter().converterData(value);
            if (null != tmp) value = tmp;
        }
        return value;
    }

    public abstract Dialect getDialect();
}
