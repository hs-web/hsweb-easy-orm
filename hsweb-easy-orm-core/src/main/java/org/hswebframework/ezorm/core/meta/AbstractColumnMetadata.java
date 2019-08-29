package org.hswebframework.ezorm.core.meta;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.*;

import java.util.HashMap;
import java.util.Map;

@Getter
@Setter
public abstract class AbstractColumnMetadata implements ColumnMetadata {
    protected String name;
    protected String alias;
    protected String comment;
    protected Class javaType;
    protected DictionaryCodec dictionaryCodec;
    protected ValueCodec valueCodec = DefaultValueCodec.INSTANCE;
    protected DefaultValue defaultValue;
    protected Validator validator;
    protected Map<String, Object> properties = new HashMap<>();

    @Override
    public String getAlias() {
        if (alias == null) {
            alias = name;
        }
        return alias;
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
}
