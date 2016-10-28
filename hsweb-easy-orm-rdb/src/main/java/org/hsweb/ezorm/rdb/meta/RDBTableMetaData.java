package org.hsweb.ezorm.rdb.meta;


import org.hsweb.ezorm.core.*;
import org.hsweb.ezorm.core.meta.TableMetaData;
import org.hsweb.ezorm.rdb.exception.TriggerException;
import org.hsweb.ezorm.rdb.meta.expand.SimplePropertyWrapper;

import java.io.Serializable;
import java.util.*;

/**
 * 表结构定义实体
 */
public class RDBTableMetaData implements TableMetaData, Serializable, Cloneable {
    private boolean locked = false;
    //表名称
    private String name;
    //表别名,如果指定了别名,查询结果将使用别名进行封装
    private String alias;
    //备注
    private String comment;
    //主键
    private Set<String>                    primaryKeys           = new HashSet<>();
    //表字段
    private Map<String, RDBColumnMetaData> fieldMetaDataMap      = new LinkedHashMap<>();
    private Map<String, RDBColumnMetaData> aliasFieldMetaDataMap = new LinkedHashMap<>();
    //数据库定义实体
    private RDBDatabaseMetaData databaseMetaData;
    private Validator           validator;
    private Set<Correlation>     correlations  = new LinkedHashSet<>();
    private Map<String, Object>  properties    = new HashMap<>();
    private Map<String, Trigger> triggerBase   = new HashMap<>();
    private ObjectWrapper        objectWrapper = null;

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
        checkWrite();
        this.name = name;
    }

    public void on(String triggerName, Trigger trigger) {
        checkWrite();
        triggerBase.put(triggerName, trigger);
    }

    public void on(String triggerName, Map<String, Object> context) throws TriggerException {
        if (triggerIsSupport(triggerName)) {
            Trigger trigger = triggerBase.get(triggerName);
            trigger.execute(context);
        }
    }

    @Override
    public RDBDatabaseMetaData getDatabase() {
        return databaseMetaData;
    }

    @Override
    public Set<RDBColumnMetaData> getColumns() {
        return new LinkedHashSet<>(fieldMetaDataMap.values());
    }

    public void removeColumn(String name) {
        fieldMetaDataMap.remove(name);
        aliasFieldMetaDataMap.remove(name);
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

    public String getAlias() {
        if (alias == null) alias = name;
        return alias;
    }

    public RDBColumnMetaData findColumn(String name) {
        if (name == null) return null;
        if (name.contains(".")) {
            String[] tmp = name.split("[.]");
            RDBTableMetaData metaData = databaseMetaData.getTable(tmp[0]);
            if (metaData == null) {
                Correlation correlation = getCorrelation(tmp[0]);
                if (correlation != null) {
                    metaData = databaseMetaData.getTable(correlation.getTargetTable());
                }
            }
            if (metaData != null) return metaData.findColumn(tmp[1]);
            return null;
        }
        RDBColumnMetaData metaData = fieldMetaDataMap.get(name);
        if (metaData == null)
            metaData = aliasFieldMetaDataMap.get(name);
        return metaData;
    }

    public boolean renameColumn(String old, String newName) {
        RDBColumnMetaData oldField = fieldMetaDataMap.get(old);
        if (oldField != null) {
            fieldMetaDataMap.remove(old);
            fieldMetaDataMap.put(newName, oldField);
            oldField.setName(newName);
            return true;
        }
        return false;
    }

    public void setAlias(String alias) {
        this.alias = alias;
    }

    public String getComment() {
        return comment;
    }

    @Override
    public RDBColumnMetaData getColumn(String name) {
        RDBColumnMetaData metaData = fieldMetaDataMap.get(name);
        if (metaData == null) metaData = aliasFieldMetaDataMap.get(name);
        return metaData;
    }

    public void setComment(String comment) {
        this.comment = comment;
    }

    public Correlation getCorrelation(String target) {
        for (Correlation correlation : correlations) {
            if (correlation.getAlias().equals(target))
                return correlation;
        }
        for (Correlation correlation : correlations) {
            if (correlation.getTargetTable().equals(target))
                return correlation;
        }
        return null;
    }

    public RDBDatabaseMetaData getDatabaseMetaData() {
        return databaseMetaData;
    }

    public void setDatabaseMetaData(RDBDatabaseMetaData databaseMetaData) {
        checkWrite();
        this.databaseMetaData = databaseMetaData;
    }

    public Correlation addCorrelation(Correlation correlation) {
        checkWrite();
        correlation.setIndex(correlations.size());
        correlations.add(correlation);
        return correlation;
    }

    public Set<Correlation> getCorrelations() {
        return correlations;
    }

    public void setCorrelations(Set<Correlation> correlations) {
        this.correlations = correlations;
    }

    public Map<String, Object> getProperties() {
        return properties;
    }

    public void setProperties(Map<String, Object> properties) {
        this.properties = properties;
    }

    public Set<String> getPrimaryKeys() {
        return primaryKeys;
    }

    public void setPrimaryKeys(Set<String> primaryKeys) {
        checkWrite();
        this.primaryKeys = primaryKeys;
    }

    public Validator getValidator() {
        return validator;
    }

    public void setValidator(Validator validator) {
        checkWrite();
        this.validator = validator;
    }

    public RDBTableMetaData addColumn(RDBColumnMetaData rdbColumnMetaData) {
        rdbColumnMetaData.setTableMetaData(this);
        fieldMetaDataMap.put(rdbColumnMetaData.getName(), rdbColumnMetaData);
        if (!rdbColumnMetaData.getName().equals(rdbColumnMetaData.getAlias()))
            aliasFieldMetaDataMap.put(rdbColumnMetaData.getAlias(), rdbColumnMetaData);
        return this;
    }

    public void lock() {
        locked = true;
    }

    public void unloc() {
        locked = false;
    }

    public void checkWrite() {
        if (locked) throw new UnsupportedOperationException("表定义已锁定,禁止修改操作");
    }

    public void setLocked(boolean locked) {
        this.locked = locked;
    }

    @Override
    public RDBTableMetaData clone() {
        RDBTableMetaData metaData = new RDBTableMetaData();
        metaData.setName(this.name);
        metaData.setAlias(this.alias);
        metaData.setComment(this.comment);
        metaData.setValidator(this.validator);
        metaData.setProperties(properties);
        metaData.triggerBase = triggerBase;
        metaData.setLocked(false);
        correlations.forEach(correlation -> metaData.addCorrelation(correlation.clone()));
        fieldMetaDataMap.values().forEach(fieldMetaData -> metaData.addColumn(fieldMetaData.clone()));
        return metaData;
    }

    @Override
    public String toString() {
        return name + " [" + alias + "]" + "(" + comment + ")";
    }
}
