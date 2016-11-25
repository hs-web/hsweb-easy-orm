package org.hsweb.ezorm.rdb.render.support.simple;

import org.apache.commons.beanutils.BeanUtilsBean;
import org.apache.commons.beanutils.PropertyUtilsBean;
import org.hsweb.commons.StringUtils;
import org.hsweb.ezorm.rdb.executor.SQL;
import org.hsweb.ezorm.rdb.meta.RDBColumnMetaData;
import org.hsweb.ezorm.rdb.meta.RDBTableMetaData;
import org.hsweb.ezorm.core.param.InsertParam;
import org.hsweb.ezorm.rdb.render.SqlAppender;
import org.hsweb.ezorm.rdb.render.SqlRender;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class SimpleInsertSqlRender implements SqlRender<InsertParam> {
    PropertyUtilsBean propertyUtils = BeanUtilsBean.getInstance().getPropertyUtils();
    private Logger logger = LoggerFactory.getLogger(this.getClass());

    @Override
    public SQL render(RDBTableMetaData metaData, InsertParam param) {
        SqlAppender appender = new SqlAppender();
        appender.add("INSERT INTO ", metaData.getName(), " ");
        appender.add("(");
        Object data = param.getData();
        if (data == null) throw new NullPointerException("不能插入为null的数据!");
        metaData.getColumns().forEach(fieldMetaData -> {
            appender.add(fieldMetaData.getName(), ",");
        });
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
            Object o = list.get(i);
            int index = i;
            if (index > 0) appender.add(",");
            appender.add("(");
            metaData.getColumns().forEach(fieldMetaData -> {
                String dataProperty = fieldMetaData.getAlias();
                Object value = null;
                try {
                    try {
                        value = propertyUtils.getProperty(o, dataProperty);
                    } catch (Exception e) {
                    }
                    if (value == null) {
                        value = propertyUtils.getProperty(o, fieldMetaData.getName());
                        if (value != null) dataProperty = fieldMetaData.getName();
                    }
                } catch (Exception e) {
                   // logger.debug("get property error", e);
                }
                if (value == null) {
                    value = fieldMetaData.getProperty("default-value").getValue();
                    if (logger.isInfoEnabled() && value != null)
                        logger.info("{}将使用默认值[default-value]:{}", dataProperty, value);
                }
                if (value != null && fieldMetaData.getValueConverter() != null) {
                    Object new_value = fieldMetaData.getValueConverter().getData(value);
                    if (fieldMetaData.getOptionConverter() != null) {
                        new_value = fieldMetaData.getOptionConverter().converterData(new_value);
                    }
                    if (value != new_value && !value.equals(new_value))
                        try {
                            propertyUtils.setProperty(o, dataProperty, new_value);
                        } catch (Exception e) {
                            logger.warn("未成功完成属性转换", e);
                        }
                }
                appender.addAll(getParamString(StringUtils.concat("data[", index, "].", dataProperty), fieldMetaData));
                appender.add(",");
            });
            appender.removeLast();
            appender.add(")");
        }
        InsertParam paramProxy = new InsertParam();
        paramProxy.setData(list);
        return new SimpleSQL(appender.toString(), paramProxy);
    }

    protected SqlAppender getParamString(String paramName, RDBColumnMetaData rdbColumnMetaData) {
        return new SqlAppender().add("#{", paramName, "}");
    }
}
