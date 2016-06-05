package og.hsweb.ezorm.meta;

import og.hsweb.ezorm.meta.converter.DefaultValueConverter;
import og.hsweb.ezorm.meta.expand.OptionConverter;
import og.hsweb.ezorm.meta.expand.PropertyWrapper;
import og.hsweb.ezorm.meta.expand.SimplePropertyWrapper;
import og.hsweb.ezorm.meta.expand.ValueConverter;
import org.apache.commons.beanutils.BeanUtils;

import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.sql.JDBCType;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Created by zhouhao on 16-5-17.
 */
public class FieldMetaData implements Serializable, Cloneable {
    private static final DefaultValueConverter DEFAULT_VALUE_CONVERTER = new DefaultValueConverter();
    private String name;

    private String alias;

    private String comment;

    private String dataType;

    private JDBCType jdbcType;

    private Class javaType;

    private TableMetaData tableMetaData;

    private OptionConverter optionConverter;

    private ValueConverter valueConverter = DEFAULT_VALUE_CONVERTER;

    private Set<String> validator;

    private Map<String, Object> properties = new HashMap<>();

    public PropertyWrapper getProperty(String name) {
        return new SimplePropertyWrapper(properties.get(name));
    }

    public PropertyWrapper getProperty(String name, Object defaultValue) {
        return new SimplePropertyWrapper(properties.getOrDefault(name, defaultValue));
    }

    public <T> T setProperty(String property, T value) {
        properties.put(property, value);
        return value;
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

    public void setAlias(String alias) {
        this.alias = alias;
    }

    public String getComment() {
        return comment;
    }

    public void setComment(String comment) {
        this.comment = comment;
    }

    public String getDataType() {
        return dataType;
    }

    public void setDataType(String dataType) {
        this.dataType = dataType;
    }

    public Class getJavaType() {
        return javaType;
    }

    public void setJavaType(Class javaType) {
        this.javaType = javaType;
    }

    public TableMetaData getTableMetaData() {
        return tableMetaData;
    }

    public void setTableMetaData(TableMetaData tableMetaData) {
        this.tableMetaData = tableMetaData;
    }

    public String getFullName() {
        return tableMetaData.getName() + "." + getName();
    }

    public String getFullAliasName() {
        return tableMetaData.getAlias() + "." + getAlias();
    }

    public JDBCType getJdbcType() {
        return jdbcType;
    }

    public void setJdbcType(JDBCType jdbcType) {
        this.jdbcType = jdbcType;
    }

    public Set<String> getValidator() {
        return validator;
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

    public OptionConverter getOptionConverter() {
        return optionConverter;
    }

    public void setOptionConverter(OptionConverter optionConverter) {
        this.optionConverter = optionConverter;
    }

    public ValueConverter getValueConverter() {
        return valueConverter;
    }

    public void setValueConverter(ValueConverter valueConverter) {
        this.valueConverter = valueConverter;
    }

    @Override
    public FieldMetaData clone() {
        FieldMetaData fieldMetaData = new FieldMetaData();
        fieldMetaData.name = name;
        fieldMetaData.alias = alias;
        fieldMetaData.comment = comment;
        fieldMetaData.javaType = javaType;
        fieldMetaData.jdbcType = jdbcType;
        fieldMetaData.dataType = dataType;
        fieldMetaData.properties = properties;
        fieldMetaData.optionConverter = optionConverter;
        fieldMetaData.tableMetaData = tableMetaData;
        return fieldMetaData;
    }
}
