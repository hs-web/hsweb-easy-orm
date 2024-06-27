package org.hswebframework.ezorm.core.meta;


import org.hswebframework.ezorm.core.*;


public interface ColumnMetadata extends FeatureSupportedMetadata, ObjectMetadata, Cloneable {
    String getName();

    String getRealName();

    String getAlias();

    String getComment();

    Class<?> getJavaType();

    ValueCodec<?,?> getValueCodec();

    DictionaryCodec getDictionaryCodec();

    DefaultValue getDefaultValue();

    PropertyWrapper getProperty(String property);

    PropertyWrapper getProperty(String property, Object defaultValue);

    PropertyWrapper setProperty(String property, Object value);

}
