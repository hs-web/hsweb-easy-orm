package org.hsweb.ezorm.meta;


import org.hsweb.ezorm.exception.TriggerException;
import org.hsweb.ezorm.meta.expand.PropertyWrapper;
import org.hsweb.ezorm.meta.expand.SimplePropertyWrapper;
import org.hsweb.ezorm.meta.expand.Trigger;
import org.hsweb.ezorm.meta.expand.Validator;

import java.io.Serializable;
import java.util.*;

/**
 * 表结构定义实体
 * Created by zhouhao on 16-5-17.
 */
public class TableMetaData implements Serializable, Cloneable {
    private boolean locked = false;
    //表名称
    private String name;
    //表别名,如果指定了别名,查询结果将使用别名进行封装
    private String alias;
    //备注
    private String comment;
    //主键
    private Set<String> primaryKeys = new HashSet<>();
    //表字段
    private Map<String, FieldMetaData> fieldMetaDataMap = new LinkedHashMap<>();
    private Map<String, FieldMetaData> aliasFieldMetaDataMap = new LinkedHashMap<>();
    //数据库定义实体
    private DatabaseMetaData databaseMetaData;
    private Validator validator;
    private Set<Correlation> correlations = new LinkedHashSet<>();
    private Map<String, Object> properties = new HashMap<>();
    private Map<String, Trigger> triggerBase = new HashMap<>();

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

    public void removeField(String name) {
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

    public <T> T setProperty(String property, T value) {
        properties.put(property, value);
        return value;
    }

    public String getAlias() {
        if (alias == null) alias = name;
        return alias;
    }

    public FieldMetaData findFieldByName(String name) {
        if (name == null) return null;
        if (name.contains(".")) {
            String[] tmp = name.split("[.]");
            TableMetaData metaData = databaseMetaData.getTable(tmp[0]);
            if (metaData == null) {
                Correlation correlation = getCorrelation(tmp[0]);
                if (correlation != null) {
                    metaData = databaseMetaData.getTable(correlation.getTargetTable());
                }
            }
            if (metaData != null) return metaData.findFieldByName(tmp[1]);
            return null;
        }
        FieldMetaData metaData = fieldMetaDataMap.get(name);
        if (metaData == null)
            metaData = aliasFieldMetaDataMap.get(name);
        return metaData;
    }

    public boolean renameField(String old, String newName) {
        FieldMetaData oldField = fieldMetaDataMap.get(old);
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

    public void setComment(String comment) {
        this.comment = comment;
    }

    public Set<FieldMetaData> getFields() {
        return new LinkedHashSet<>(fieldMetaDataMap.values());
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

    public DatabaseMetaData getDatabaseMetaData() {
        return databaseMetaData;
    }

    public void setDatabaseMetaData(DatabaseMetaData databaseMetaData) {
        checkWrite();
        this.databaseMetaData = databaseMetaData;
    }

    public Correlation addCorrelation(Correlation correlation) {
        checkWrite();
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

    public TableMetaData addField(FieldMetaData fieldMetaData) {
        fieldMetaData.setTableMetaData(this);
        fieldMetaDataMap.put(fieldMetaData.getName(), fieldMetaData);
        if (!fieldMetaData.getName().equals(fieldMetaData.getAlias()))
            aliasFieldMetaDataMap.put(fieldMetaData.getAlias(), fieldMetaData);
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
    public TableMetaData clone() {
        TableMetaData metaData = new TableMetaData();
        metaData.setName(this.name);
        metaData.setAlias(this.alias);
        metaData.setComment(this.comment);
        metaData.setValidator(this.validator);
        metaData.setProperties(properties);
        metaData.triggerBase = triggerBase;
        metaData.setLocked(false);
        correlations.forEach(correlation -> metaData.addCorrelation(correlation.clone()));
        fieldMetaDataMap.values().forEach(fieldMetaData -> metaData.addField(fieldMetaData.clone()));
        return metaData;
    }

    @Override
    public String toString() {
        return name + " [" + alias + "]" + "(" + comment + ")";
    }
}
