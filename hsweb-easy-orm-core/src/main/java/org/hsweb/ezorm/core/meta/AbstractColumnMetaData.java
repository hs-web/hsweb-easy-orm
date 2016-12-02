package org.hsweb.ezorm.core.meta;

import org.hsweb.ezorm.core.OptionConverter;
import org.hsweb.ezorm.core.PropertyWrapper;
import org.hsweb.ezorm.core.SimplePropertyWrapper;
import org.hsweb.ezorm.core.ValueConverter;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public abstract class AbstractColumnMetaData implements ColumnMetaData {
    protected String          name;
    protected String          alias;
    protected String          comment;
    protected Class           javaType;
    protected TableMetaData   tableMetaData;
    protected OptionConverter optionConverter;
    protected ValueConverter  valueConverter;
    protected Set<String>     validator;
    protected Map<String, Object> properties = new HashMap<>();

    @Override
    public String getName() {
        return name;
    }

    @Override
    public String getAlias() {
        if (alias == null) alias = name;
        return alias;
    }

    @Override
    public String getComment() {
        if (comment == null) comment = "";
        return comment;
    }

    @Override
    public Class getJavaType() {
        return javaType;
    }

    @Override
    public Set<String> getValidator() {
        return validator;
    }

    @Override
    public <T extends TableMetaData> T getTableMetaData() {
        return (T) tableMetaData;
    }

    @Override
    public ValueConverter getValueConverter() {
        return valueConverter;
    }

    @Override
    public OptionConverter getOptionConverter() {
        return optionConverter;
    }

    @Override
    public PropertyWrapper getProperty(String property) {
        return new SimplePropertyWrapper(properties.get(property));
    }

    @Override
    public PropertyWrapper getProperty(String property, Object defaultValue) {
        Object value = properties.get(property);
        return new SimplePropertyWrapper(value == null ? defaultValue : value);
    }

    @Override
    public PropertyWrapper setProperty(String property, Object value) {
        return new SimplePropertyWrapper(properties.put(property, value));
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setAlias(String alias) {
        this.alias = alias;
    }

    public void setComment(String comment) {
        this.comment = comment;
    }

    public void setJavaType(Class javaType) {
        this.javaType = javaType;
    }

    public void setTableMetaData(TableMetaData tableMetaData) {
        this.tableMetaData = tableMetaData;
    }

    public void setOptionConverter(OptionConverter optionConverter) {
        this.optionConverter = optionConverter;
    }

    public void setValueConverter(ValueConverter valueConverter) {
        this.valueConverter = valueConverter;
    }

    public void setValidator(Set<String> validator) {
        this.validator = validator;
    }

    public Map<String, Object> getProperties() {
        return properties;
    }

    public void setProperties(Map<String, Object> properties) {
        this.properties = properties;
    }

    @Override
    public abstract AbstractColumnMetaData clone();
}
