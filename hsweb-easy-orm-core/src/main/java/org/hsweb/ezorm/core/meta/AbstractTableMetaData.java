package org.hsweb.ezorm.core.meta;

import org.hsweb.ezorm.core.*;

import java.util.*;

public abstract class AbstractTableMetaData<C extends AbstractColumnMetaData> implements TableMetaData {
    //表名称
    protected String               name                   = null;
    //表别名,如果指定了别名,查询结果将使用别名进行封装
    protected String               alias                  = null;
    //备注
    protected String               comment                = null;
    //表字段
    protected Map<String, C>       columnMetaDataMap      = new LinkedHashMap<>();
    protected Map<String, C>       aliasColumnMetaDataMap = new LinkedHashMap<>();
    protected Validator            validator              = null;
    protected ObjectWrapper        objectWrapper          = null;
    protected Map<String, Object>  properties             = new HashMap<>();
    protected Map<String, Trigger> triggerBase            = new HashMap<>();

    @Override
    public <T> ObjectWrapper<T> getObjectWrapper() {
        return objectWrapper;
    }

    public void setObjectWrapper(ObjectWrapper objectWrapper) {
        this.objectWrapper = objectWrapper;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getAlias() {
        if (alias == null) alias = name;
        return alias;
    }

    public <T extends AbstractTableMetaData<C>> T addColumn(C columnMetaData) {
        columnMetaData.setTableMetaData(this);
        columnMetaDataMap.put(columnMetaData.getName(), columnMetaData);
        if (!columnMetaData.getName().equals(columnMetaData.getAlias()))
            aliasColumnMetaDataMap.put(columnMetaData.getAlias(), columnMetaData);
        return (T) this;
    }

    public void setAlias(String alias) {
        this.alias = alias;
    }

    public void on(String triggerName, Trigger trigger) {
        triggerBase.put(triggerName, trigger);
    }

    public void on(String triggerName, Map<String, Object> context) {
        if (triggerIsSupport(triggerName)) {
            Trigger trigger = triggerBase.get(triggerName);
            trigger.execute(context);
        }
    }

    @Override
    public Set<C> getColumns() {
        return new LinkedHashSet<>(columnMetaDataMap.values());
    }

    public void removeColumn(String name) {
        columnMetaDataMap.remove(name);
        aliasColumnMetaDataMap.remove(name);
    }

    public boolean triggerIsSupport(String name) {
        return triggerBase.containsKey(name);
    }

    public PropertyWrapper getProperty(String name) {
        return new SimplePropertyWrapper(properties.get(name));
    }

    public PropertyWrapper getProperty(String name, Object defaultValue) {
        return new SimplePropertyWrapper(properties.getOrDefault(name, defaultValue));
    }

    public PropertyWrapper removeProperty(String name) {
        return new SimplePropertyWrapper(properties.remove(name));
    }

    public PropertyWrapper setProperty(String property, Object value) {
        return new SimplePropertyWrapper(properties.put(property, value));
    }


    public abstract C findColumn(String name);

    public String getComment() {
        return comment;
    }

    @Override
    public C getColumn(String name) {
        C metaData = columnMetaDataMap.get(name);
        if (metaData == null) metaData = aliasColumnMetaDataMap.get(name);
        return metaData;
    }

    public void setComment(String comment) {
        this.comment = comment;
    }

    public Map<String, Object> getProperties() {
        return properties;
    }

    public void setProperties(Map<String, Object> properties) {
        this.properties = properties;
    }

    public Validator getValidator() {
        return validator;
    }

    public void setValidator(Validator validator) {
        this.validator = validator;
    }

    @Override
    public String toString() {
        return name + " [" + alias + "]" + "(" + comment + ")";
    }
}
