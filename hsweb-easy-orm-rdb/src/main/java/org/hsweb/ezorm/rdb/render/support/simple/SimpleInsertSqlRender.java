package org.hsweb.ezorm.rdb.render.support.simple;

import org.apache.commons.beanutils.BeanUtilsBean;
import org.apache.commons.beanutils.PropertyUtilsBean;
import org.hsweb.commons.StringUtils;
import org.hsweb.ezorm.core.param.InsertParam;
import org.hsweb.ezorm.rdb.executor.BindSQL;
import org.hsweb.ezorm.rdb.executor.SQL;
import org.hsweb.ezorm.rdb.meta.RDBColumnMetaData;
import org.hsweb.ezorm.rdb.meta.RDBTableMetaData;
import org.hsweb.ezorm.rdb.render.SqlAppender;
import org.hsweb.ezorm.rdb.render.SqlRender;
import org.hsweb.ezorm.rdb.render.dialect.Dialect;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.InvocationTargetException;
import java.util.*;

public class SimpleInsertSqlRender implements SqlRender<InsertParam> {
    PropertyUtilsBean propertyUtils = BeanUtilsBean.getInstance().getPropertyUtils();
    private Logger logger = LoggerFactory.getLogger(this.getClass());

    protected SimpleSQL createSingleSql(RDBTableMetaData metaData, Object data, Object param, String valueExpressionPrefix) {
        Dialect dialect = metaData.getDatabaseMetaData().getDialect();
        SqlAppender appender = new SqlAppender();
        List<String> columns = new ArrayList<>();
        List<String> valuesExpression = new ArrayList<>();
        Map<String, Object> mapValue = new HashMap<>();

        metaData.getColumns().forEach(column -> {
            Object value = null;
            String propertyName = null;
            try {
                value = propertyUtils.getProperty(data, propertyName = column.getAlias());
            } catch (Exception e) {
                try {
                    value = propertyUtils.getProperty(data, propertyName = column.getName());
                } catch (Exception ignore) {
                    //ignore
                }
            }
            if (value == null) {
                value = column.getProperty("default-value").getValue();
                if (logger.isInfoEnabled() && value != null)
                    logger.info("{}将使用默认值[default-value]:{}", propertyName, value);
            }
            if (value != null && column.getValueConverter() != null) {
                Object new_value = column.getValueConverter().getData(value);
                if (column.getOptionConverter() != null) {
                    new_value = column.getOptionConverter().converterData(new_value);
                }
                if (value != new_value && !value.equals(new_value)) {
                    if (logger.isDebugEnabled())
                        logger.debug("{} 转换value:{}为:{}", propertyName, value, new_value);
                    value = new_value;
                }
            }
            if (null == value) {
                return;
            }
            mapValue.put(propertyName, value);

            columns.add(dialect.buildColumnName(null, column.getName()));
            valuesExpression.add(getParamString(valueExpressionPrefix, propertyName, column).toString());
        });
        appender.add("INSERT INTO ", metaData.getName(), " (")
                .add(String.join(",", columns.toArray(new String[columns.size()])), ")VALUES(")
                .add(String.join(",", valuesExpression.toArray(new String[valuesExpression.size()])), ")");

        return new SimpleSQL(appender.toString(), mapValue);
    }


    @Override
    public SQL render(RDBTableMetaData metaData, InsertParam param) {
        Object data = Objects.requireNonNull(param.getData(), "param can not be null!");
        if (data == null) throw new NullPointerException();

        List<Object> datas = null;
        if (data instanceof Collection) {
            datas = new ArrayList<>(((Collection) data));
        } else if (data instanceof Object[]) {
            datas = Arrays.asList(((Object[]) data));
        }
        if (datas == null) {
            SimpleSQL simpleSQL = createSingleSql(metaData, data, param, "data.");
            param.setData(simpleSQL.getParams());
            simpleSQL.setParams(param);
            return simpleSQL;
        } else {
            SimpleSQL firstSql = createSingleSql(metaData, datas.get(0), param, "data[0].");
            List<BindSQL> bindSQLS = new ArrayList<>();
            List<Object> newParam = new ArrayList<>();
            newParam.add(firstSql.getParams());
            for (int i = 1; i < datas.size(); i++) {
                SimpleSQL sql = createSingleSql(metaData, datas.get(i), param, "data[" + i + "].");
                newParam.add(sql.getParams());
                sql.setParams(param);
                bindSQLS.add(new BindSQL(sql));
            }
            firstSql.setBindSQLs(bindSQLS);
            param.setData(newParam);
            firstSql.setParams(param);
            return firstSql;
        }
    }

    protected SqlAppender getParamString(String prefix, String paramName, RDBColumnMetaData rdbColumnMetaData) {
        return new SqlAppender().add("#{", prefix, paramName, "}");
    }
}
