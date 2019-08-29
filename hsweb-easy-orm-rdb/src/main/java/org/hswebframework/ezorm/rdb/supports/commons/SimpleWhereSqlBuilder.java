package org.hswebframework.ezorm.rdb.supports.commons;

import org.hswebframework.ezorm.rdb.meta.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.meta.TableOrViewMetadata;
import org.hswebframework.utils.StringUtils;
import org.hswebframework.ezorm.core.param.SqlTerm;
import org.hswebframework.ezorm.core.param.Term;
//import org.hswebframework.ezorm.rdb.meta.RDBTableMetaData_;
import org.hswebframework.ezorm.rdb.dialect.Dialect;

import java.util.*;

public abstract class SimpleWhereSqlBuilder {

    protected String getTableAlias(TableOrViewMetadata metaData, String field) {
        if (field.contains("."))
            field = field.split("[.]")[0];
        else return metaData.getAlias();
//        Correlation correlation = metaData.getCorrelation(field);
//        if (correlation != null) return correlation.getAlias();
        return metaData.getAlias();
    }

    public void buildWhere(TableOrViewMetadata metaData, String prefix,
                           List<Term> terms, List<String> appender,
                           Set<String> needSelectTable) {
        if (terms == null || terms.isEmpty()) return;
        int index = -1;
        String prefixTmp = StringUtils.concat(prefix, StringUtils.isNullOrEmpty(prefix) ? "" : ".");
        for (Term term : terms) {
            index++;
            boolean nullTerm = StringUtils.isNullOrEmpty(term.getColumn());
            RDBColumnMetadata column = metaData.findColumn(term.getColumn()).orElse(null);
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
            }
            //用于sql预编译的参数名
            prefix = StringUtils.concat(prefixTmp, "terms[", index, "]");
            //添加类型，and 或者 or
            appender.add(StringUtils.concat(" ", term.getType().toString().toUpperCase(), " "));
//            if (!term.getTerms().isEmpty()) {
//                SqlAppender that = nullTerm ? null : getDialect().buildCondition(prefix, term, column, tableAlias);
//                SqlAppender nest = null;
//                //在自定义SQL条件中可能会修改这个值,所以要重新判断
//                if (!term.getTerms().isEmpty()) {
//                    //构建嵌套的条件
//                    nest = new SqlAppender();
//                    buildWhere(metaData, prefix, term.getTerms(), nest, needSelectTable);
//                    //如果嵌套结果为空
//                    if (nest.isEmpty()) {
//                        if (!nullTerm) {
//                            appender.add(that);
//                        } else {
//                            appender.removeLast();//删除最后一个（and 或者 or）
//                        }
//                        continue;
//                    }
//                    if (nullTerm) {
//                        //删除 第一个（and 或者 or）
//                        nest.removeFirst();
//                    }
//                    appender.add("(");
//                }
//                if (that != null) {
//                    appender.add(that);
//                }
//                if (nest != null) {
//                    appender.addAll(nest);
//                    appender.add(")");
//                }
//
//            } else {
//                if (!nullTerm) {
//                    appender.add(getDialect().buildCondition(prefix, term, column, tableAlias));
//                }
//            }
        }
    }

    public abstract Dialect getDialect();
}
