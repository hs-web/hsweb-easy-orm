package org.hsweb.ezorm.render.support.simple;

import org.apache.commons.beanutils.BeanUtilsBean;
import org.apache.commons.beanutils.PropertyUtilsBean;
import org.hsweb.ezorm.executor.SQL;
import org.hsweb.ezorm.meta.FieldMetaData;
import org.hsweb.ezorm.meta.TableMetaData;
import org.hsweb.ezorm.param.Term;
import org.hsweb.ezorm.param.UpdateParam;
import org.hsweb.ezorm.render.Dialect;
import org.hsweb.ezorm.render.SqlAppender;
import org.apache.commons.beanutils.BeanUtils;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Created by zhouhao on 16-6-4.
 */
public class SimpleUpdateSqlRender extends CommonSqlRender<UpdateParam> {

    class SimpleUpdateSqlRenderProcess extends SimpleWhereSqlBuilder {
        private TableMetaData metaData;
        private UpdateParam param;
        private List<OperationField> updateField;
        private SqlAppender whereSql = new SqlAppender();
        private Set<String> conditionTable = new LinkedHashSet<>();
        PropertyUtilsBean propertyUtils = BeanUtilsBean.getInstance().getPropertyUtils();

        public SimpleUpdateSqlRenderProcess(TableMetaData metaData, UpdateParam param) {
            this.metaData = metaData;
            this.param = param;
            List<Term> terms = param.getTerms();
            terms = terms.stream().filter(term -> !term.getField().contains(".")).collect(Collectors.toList());
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
            updateField.forEach(operationField -> {
                FieldMetaData fieldMetaData = operationField.getFieldMetaData();
                if (fieldMetaData.getProperty("read-only").isTrue()) return;
                try {
                    String dataProperty = fieldMetaData.getAlias();
                    Object value = null;
                    try {
                        value = propertyUtils.getProperty(param.getData(), dataProperty);
                    } catch (Exception e) {
                    }
                    if (value == null && !fieldMetaData.getAlias().equals(fieldMetaData.getName())) {
                        dataProperty = fieldMetaData.getName();
                        value = propertyUtils.getProperty(param.getData(), dataProperty);
                    }
                    if (value == null) {
                        if (logger.isDebugEnabled())
                            logger.debug("跳过修改列:[{}], 属性[{}]为null!", fieldMetaData.getName(), fieldMetaData.getAlias());
                        return;
                    }
                    if (fieldMetaData.getValueConverter() != null) {
                        Object new_value = fieldMetaData.getValueConverter().getData(value);
                        if (fieldMetaData.getOptionConverter() != null) {
                            new_value = fieldMetaData.getOptionConverter().converterData(new_value);
                        }
                        if (value != new_value && !value.equals(new_value))
                            propertyUtils.setProperty(param.getData(), dataProperty, new_value);
                    }
                    appender.add(fieldMetaData.getName(), "=", "#{data.", dataProperty, "}", ",");
                    bytes[0]++;
                } catch (Exception e) {
                    if (logger.isDebugEnabled())
                        logger.debug("跳过修改列:[{}], 可能属性[{}]不存在!", fieldMetaData.getName(), fieldMetaData.getAlias());
                }
            });
            if (bytes[0] == 0) throw new IndexOutOfBoundsException("没有列被修改!");
            appender.removeLast();
            if (whereSql.isEmpty()) {
                throw new UnsupportedOperationException("禁止执行未设置任何条件的修改操作!");
            }
            appender.add(" WHERE ", "").addAll(whereSql);
            String sql = appender.toString();
            SimpleSQL simpleSQL = new SimpleSQL(sql, param);
            return simpleSQL;
        }

        @Override
        public Dialect getDialect() {
            return dialect;
        }
    }

    @Override
    public SQL render(TableMetaData metaData, UpdateParam param) {
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
