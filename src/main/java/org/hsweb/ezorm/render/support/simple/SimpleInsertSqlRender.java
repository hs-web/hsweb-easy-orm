package org.hsweb.ezorm.render.support.simple;

import org.apache.commons.beanutils.BeanUtilsBean;
import org.apache.commons.beanutils.PropertyUtilsBean;
import org.hsweb.ezorm.executor.SQL;
import org.hsweb.ezorm.meta.TableMetaData;
import org.hsweb.ezorm.param.InsertParam;
import org.hsweb.ezorm.render.SqlAppender;
import org.hsweb.ezorm.render.SqlRender;
import org.apache.commons.beanutils.BeanUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Created by zhouhao on 16-6-4.
 */
public class SimpleInsertSqlRender implements SqlRender<InsertParam> {
    PropertyUtilsBean propertyUtils = BeanUtilsBean.getInstance().getPropertyUtils();
    private Logger logger = LoggerFactory.getLogger(this.getClass());

    @Override
    public SQL render(TableMetaData metaData, InsertParam param) {
        SqlAppender appender = new SqlAppender();
        appender.add("INSERT INTO ", metaData.getName(), " ");
        appender.add("(");
        Object data = param.getData();
        if (data == null) throw new NullPointerException("不能插入为null的数据!");
        metaData.getFields().forEach(fieldMetaData -> {
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
            metaData.getFields().forEach(fieldMetaData -> {
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
                    logger.debug("get property error", e);
                }
                if (value == null) {
                    value = fieldMetaData.getProperty("default-value").getValue();
                    if (logger.isInfoEnabled())
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
                appender.add("#{data[", index, "].", dataProperty, "}", ",");
            });
            appender.removeLast();
            appender.add(")");
        }
        InsertParam paramProxy = new InsertParam();
        paramProxy.setData(list);
        return new SimpleSQL(appender.toString(), paramProxy);
    }
}
