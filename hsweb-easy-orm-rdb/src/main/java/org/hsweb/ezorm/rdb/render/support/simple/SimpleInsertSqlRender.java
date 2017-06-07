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

    protected SQL createSingleSql(RDBTableMetaData metaData, Object data, Object param, String valueExpressionPrefix) {
        Dialect dialect = metaData.getDatabaseMetaData().getDialect();
        SqlAppender appender = new SqlAppender();
        List<String> columns = new ArrayList<>();
        List<String> valuesExpression = new ArrayList<>();
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
            try {
                propertyUtils.setProperty(data, propertyName, value);
            } catch (NoSuchMethodException ignore) {
            } catch (Exception e) {
                logger.warn("无法设置属性:{} 值:{}", propertyName, value, e);
            }
            columns.add(dialect.buildColumnName(null, column.getName()));
            valuesExpression.add(getParamString(valueExpressionPrefix, propertyName, column).toString());
        });
        appender.add("INSERT INTO ", metaData.getName(), " (")
                .add(String.join(",", columns.toArray(new String[columns.size()])), ")VALUES(")
                .add(String.join(",", valuesExpression.toArray(new String[valuesExpression.size()])), ")");
        return new SimpleSQL(appender.toString(), param);
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
            return createSingleSql(metaData, data, param, "data.");
        } else {
            SimpleSQL simpleSQL = new SimpleSQL(createSingleSql(metaData, datas.get(0), param, "data[0].").getSql(), param);
            List<BindSQL> bindSQLS = new ArrayList<>();
            for (int i = 1; i < datas.size(); i++) {
                bindSQLS.add(new BindSQL(createSingleSql(metaData, datas.get(i), param, "data[" + i + "].")));
            }
            simpleSQL.setBindSQLs(bindSQLS);
            return simpleSQL;
        }
    }

    protected SqlAppender getParamString(String prefix, String paramName, RDBColumnMetaData rdbColumnMetaData) {
        return new SqlAppender().add("#{", prefix, paramName, "}");
    }
}
