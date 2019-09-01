package org.hswebframework.ezorm.core.meta;


import org.hswebframework.ezorm.core.*;


@SuppressWarnings("all")
public interface ColumnMetadata extends FeatureSupportedMetadata, ObjectMetadata, Cloneable {
    String getName();

    String getAlias();

    String getComment();

    Class getJavaType();

    ValueCodec getValueCodec();

    DictionaryCodec getDictionaryCodec();

    DefaultValue getDefaultValue();

    Validator getValidator();

    PropertyWrapper getProperty(String property);

    PropertyWrapper getProperty(String property, Object defaultValue);

    PropertyWrapper setProperty(String property, Object value);

}
