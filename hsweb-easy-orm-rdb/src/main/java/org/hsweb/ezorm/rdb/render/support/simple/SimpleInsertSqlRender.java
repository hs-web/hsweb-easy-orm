package org.hsweb.ezorm.rdb.render.support.simple;

import org.apache.commons.beanutils.BeanUtilsBean;
import org.apache.commons.beanutils.PropertyUtilsBean;
import org.hsweb.commons.StringUtils;
import org.hsweb.ezorm.core.param.InsertParam;
import org.hsweb.ezorm.rdb.executor.SQL;
import org.hsweb.ezorm.rdb.meta.RDBColumnMetaData;
import org.hsweb.ezorm.rdb.meta.RDBTableMetaData;
import org.hsweb.ezorm.rdb.render.SqlAppender;
import org.hsweb.ezorm.rdb.render.SqlRender;
import org.hsweb.ezorm.rdb.render.dialect.Dialect;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

public class SimpleInsertSqlRender implements SqlRender<InsertParam> {
    PropertyUtilsBean propertyUtils = BeanUtilsBean.getInstance().getPropertyUtils();
    private Logger logger = LoggerFactory.getLogger(this.getClass());

    @Override
    public SQL render(RDBTableMetaData metaData, InsertParam param) {
        SqlAppender appender = new SqlAppender();
        List<Map<String, Object>> mapData = new ArrayList<>();

        appender.add("INSERT INTO ", metaData.getName(), " ");
        appender.add("(");
        Object data = param.getData();
        if (data == null) throw new NullPointerException("不能插入为null的数据!");
        Dialect dialect = metaData.getDatabaseMetaData().getDialect();
        metaData.getColumns().forEach(column -> appender.add(dialect.buildColumnName(null, column.getName()), ","));
        appender.removeLast();
        appender.add(")values");
        List<Object> list = new ArrayList<>();
        if (data instanceof Collection) {
            list.addAll(((Collection) data));
        } else {
            list.add(data);
        }
        param.setData(list);
        for (int i = 0; i < list.size(); i++) {
            Object obj = list.get(i);
            int index = i;
            if (index > 0) appender.add(",");
            appender.add("(");
            Map<String, Object> objProxy = new HashMap<>();
            metaData.getColumns().forEach(column -> {
                String dataProperty = column.getAlias();
                Object value = null;
                try {
                    try {
                        value = propertyUtils.getProperty(obj, dataProperty);
                    } catch (Exception e) {
                    }
                    if (value == null) {
                        value = propertyUtils.getProperty(obj, column.getName());
                        if (value != null) dataProperty = column.getName();
                    }
                } catch (Exception e) {
                    // logger.debug("get property error", e);
                }
                if (value == null) {
                    value = column.getProperty("default-value").getValue();
                    if (logger.isInfoEnabled() && value != null)
                        logger.info("{}将使用默认值[default-value]:{}", dataProperty, value);
                }
                if (value != null && column.getValueConverter() != null) {
                    Object new_value = column.getValueConverter().getData(value);
                    if (column.getOptionConverter() != null) {
                        new_value = column.getOptionConverter().converterData(new_value);
                    }
                    if (value != new_value && !value.equals(new_value)) {
                        value = new_value;
                    }
                }
                objProxy.put(dataProperty, value);
                appender.addAll(getParamString(StringUtils.concat("data[", index, "]."), dataProperty, column));
                appender.add(",");
            });
            mapData.add(objProxy);
            appender.removeLast();
            appender.add(")");
        }
        InsertParam paramProxy = new InsertParam();
        paramProxy.setData(mapData);
        return new SimpleSQL(appender.toString(), paramProxy);
    }

    protected SqlAppender getParamString(String prefix, String paramName, RDBColumnMetaData rdbColumnMetaData) {
        return new SqlAppender().add("#{", prefix, paramName, "}");
    }
}
