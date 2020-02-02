package org.hswebframework.ezorm.rdb.render.support.simple;

import org.apache.commons.beanutils.BeanUtilsBean;
import org.apache.commons.beanutils.PropertyUtilsBean;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.UpdateParam;
import org.hswebframework.ezorm.rdb.executor.SQL;
import org.hswebframework.ezorm.rdb.meta.RDBColumnMetaData;
import org.hswebframework.ezorm.rdb.meta.RDBTableMetaData;
import org.hswebframework.ezorm.rdb.render.Sql;
import org.hswebframework.ezorm.rdb.render.SqlAppender;
import org.hswebframework.ezorm.rdb.render.dialect.Dialect;
import org.hswebframework.ezorm.rdb.utils.PropertiesUtils;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Created by zhouhao on 16-6-4.
 */
@SuppressWarnings("all")
public class SimpleUpdateSqlRender extends CommonSqlRender<UpdateParam> {

    class SimpleUpdateSqlRenderProcess extends SimpleWhereSqlBuilder {
        private RDBTableMetaData metaData;
        private UpdateParam param;
        private List<OperationColumn> updateField;
        private SqlAppender whereSql = new SqlAppender();
        private Set<String> conditionTable = new LinkedHashSet<>();
        PropertyUtilsBean propertyUtils = BeanUtilsBean.getInstance().getPropertyUtils();

        public SimpleUpdateSqlRenderProcess(RDBTableMetaData metaData, UpdateParam param) {
            this.metaData = metaData;
            this.param = param;//.clone();
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
            appender.add("UPDATE ", metaData.getFullName(), " ", metaData.getAlias(), " SET ");
            byte[] bytes = new byte[1];
            Map<String, Object> valueProxy = new HashMap<>();
            updateField.forEach(operationColumn -> {
                RDBColumnMetaData column = operationColumn.getRDBColumnMetaData();
                if (column.getProperty("read-only").isTrue()) {
                    return;
                }
                String dataProperty = null;
                Object value = PropertiesUtils.getProperty(param.getData(), dataProperty = column.getAlias()).orElse(null);
                if (value == null && !column.getAlias().equals(column.getName())) {
                    value = PropertiesUtils.getProperty(param.getData(), dataProperty = column.getName()).orElse(null);
                }
                boolean alwaysUpdate = column.getProperty("update-always", false).isTrue();

                if (value == null) {
                    if (alwaysUpdate && column.getDefaultValue() != null) {
                        value = column.getDefaultValue().get();
                    }
                    if (value == null) {
                        if (logger.isInfoEnabled()) {
                            logger.info("跳过修改表:[{}]中的列:[{}], 属性[{}]为null!", metaData.getFullName(), column.getName(), column.getAlias());
                        }
                        return;
                    }
                }
                if (!(value instanceof Sql) &&column.getValueConverter() != null) {
                    Object newValue = column.getValueConverter().getData(value);
                    if (column.getOptionConverter() != null) {
                        newValue = column.getOptionConverter().converterData(newValue);
                    }
                    if (value != newValue && !value.equals(newValue)) {
                        value = newValue;
                    }
                }
                valueProxy.put(dataProperty, value);
                appender.add(dialect.buildColumnName(null, column.getName()), "=");
                if (value instanceof Sql) {
                    appender.add(((Sql) value).getSql());
                } else {
                    appender.addAll(getParamString("data.".concat(dataProperty), column));
                }
                appender.add(",");
                bytes[0]++;
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
