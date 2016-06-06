package org.hsweb.ezorm.render.support.simple;

import org.hsweb.ezorm.executor.SQL;
import org.hsweb.ezorm.meta.TableMetaData;
import org.hsweb.ezorm.param.InsertParam;
import org.hsweb.ezorm.render.SqlAppender;
import org.hsweb.ezorm.render.SqlRender;
import org.apache.commons.beanutils.BeanUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Created by zhouhao on 16-6-4.
 */
public class SimpleInsertSqlRender implements SqlRender<InsertParam> {

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
        Object o = list.get(0);
        for (int i = 0; i < list.size(); i++) {
            int index = i;
            if (index > 0) appender.add(",");
            appender.add("(");
            metaData.getFields().forEach(fieldMetaData -> {
                String dataProperty = fieldMetaData.getAlias();
                Object value = null;
                try {
                    if (!fieldMetaData.getAlias().equals(fieldMetaData.getName())) {
                        value = BeanUtils.getProperty(o, fieldMetaData.getAlias());
                        if (value == null) {
                            value = BeanUtils.getProperty(o, fieldMetaData.getName());
                            if (value != null) dataProperty = fieldMetaData.getName();
                        }
                    }
                } catch (Exception e) {
                }
                appender.add("#{data[", index, "].", dataProperty, "}", ",");
            });
            appender.removeLast();
            appender.add(")");
        }
        InsertParam paramProxy = new InsertParam();
        paramProxy.setData(list);
        return new SimpleSQL( appender.toString(), paramProxy);
    }
}
