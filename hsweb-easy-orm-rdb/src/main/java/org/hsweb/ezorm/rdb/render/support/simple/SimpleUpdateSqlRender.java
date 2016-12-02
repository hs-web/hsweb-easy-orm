package org.hsweb.ezorm.rdb.render.support.simple;

import org.apache.commons.beanutils.BeanUtilsBean;
import org.apache.commons.beanutils.PropertyUtilsBean;
import org.hsweb.ezorm.core.param.Term;
import org.hsweb.ezorm.core.param.UpdateParam;
import org.hsweb.ezorm.rdb.executor.SQL;
import org.hsweb.ezorm.rdb.meta.RDBColumnMetaData;
import org.hsweb.ezorm.rdb.meta.RDBTableMetaData;
import org.hsweb.ezorm.rdb.render.SqlAppender;
import org.hsweb.ezorm.rdb.render.dialect.Dialect;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Created by zhouhao on 16-6-4.
 */
public class SimpleUpdateSqlRender extends CommonSqlRender<UpdateParam> {

    class SimpleUpdateSqlRenderProcess extends SimpleWhereSqlBuilder {
        private RDBTableMetaData      metaData;
        private UpdateParam           param;
        private List<OperationColumn> updateField;
        private SqlAppender whereSql       = new SqlAppender();
        private Set<String> conditionTable = new LinkedHashSet<>();
        PropertyUtilsBean propertyUtils = BeanUtilsBean.getInstance().getPropertyUtils();

        public SimpleUpdateSqlRenderProcess(RDBTableMetaData metaData, UpdateParam param) {
            this.metaData = metaData;
            this.param = param.clone();
            List<Term> terms = param.getTerms();
            terms = terms.stream().filter(term -> term.getColumn() == null || !term.getColumn().contains(".")).collect(Collectors.toList());
            param.setTerms(terms);
            //解析要操作的字段
            this.updateField = parseOperationField(metaData, param);
            //解析查询条件
            buildWhere(metaData, "", param.getTerms(), whereSql, conditionTable);
            if (!whereSql.isEmpty()) whereSql.removeFirst();
        }

        public SQL process() {
            SqlAppender appender = new SqlAppender();
            appender.add("UPDATE ", metaData.getName(), " ", metaData.getAlias(), " SET ");
            byte[] bytes = new byte[1];
            Map<String, Object> valueProxy = new HashMap<>();
            updateField.forEach(operationColumn -> {
                RDBColumnMetaData column = operationColumn.getRDBColumnMetaData();
                if (column.getProperty("read-only").isTrue()) return;
                try {
                    String dataProperty = column.getAlias();
                    Object value = null;
                    try {
                        value = propertyUtils.getProperty(param.getData(), dataProperty);
                    } catch (Exception e) {
                    }
                    if (value == null && !column.getAlias().equals(column.getName())) {
                        dataProperty = column.getName();
                        try {
                            value = propertyUtils.getProperty(param.getData(), dataProperty);
                        } catch (Exception e) {
                        }
                    }
                    if (value == null) {
                        if (logger.isInfoEnabled())
                            logger.info("跳过修改列:[{}], 属性[{}]为null!", column.getName(), column.getAlias());
                        return;
                    }
                    if (column.getValueConverter() != null) {
                        Object new_value = column.getValueConverter().getData(value);
                        if (column.getOptionConverter() != null) {
                            new_value = column.getOptionConverter().converterData(new_value);
                        }
                        if (value != new_value && !value.equals(new_value)) {
                            // propertyUtils.setProperty(param.getData(), dataProperty, new_value);
                            value = new_value;
                        }
                    }
                    valueProxy.put(dataProperty, value);
                    appender.add(dialect.buildColumnName(null, column.getName()), "=")
                            .addAll(getParamString("data.".concat(dataProperty), column));
                    appender.add(",");
                    bytes[0]++;
                } catch (Exception e) {
                    if (logger.isInfoEnabled())
                        logger.info("跳过修改列:[{}], 可能属性[{}]不存在!", column.getName(), column.getAlias());
                }
            });
            if (bytes[0] == 0) throw new IndexOutOfBoundsException("没有列被修改!");
            appender.removeLast();
            if (whereSql.isEmpty()) {
                throw new UnsupportedOperationException("禁止执行未设置任何条件的修改操作!");
            }
            appender.add(" WHERE ", "").addAll(whereSql);
            String sql = appender.toString();
            param.setData(valueProxy);
            SimpleSQL simpleSQL = new SimpleSQL(sql, param);
            return simpleSQL;
        }

        @Override
        public Dialect getDialect() {
            return dialect;
        }
    }

    protected SqlAppender getParamString(String paramName, RDBColumnMetaData rdbColumnMetaData) {
        return new SqlAppender().add("#{", paramName, "}");
    }

    @Override
    public SQL render(RDBTableMetaData metaData, UpdateParam param) {
        return new SimpleUpdateSqlRenderProcess(metaData, param).process();
    }

    public SimpleUpdateSqlRender(Dialect dialect) {
        this.dialect = dialect;
    }

    private Dialect dialect;

    public Dialect getDialect() {
        return dialect;
    }

    public void setDialect(Dialect dialect) {
        this.dialect = dialect;
    }
}
