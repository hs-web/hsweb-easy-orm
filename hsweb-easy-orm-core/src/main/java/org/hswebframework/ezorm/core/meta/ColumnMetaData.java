package org.hswebframework.ezorm.core.meta;


import org.hswebframework.ezorm.core.DefaultValue;
import org.hswebframework.ezorm.core.DictionaryCodec;
import org.hswebframework.ezorm.core.PropertyWrapper;
import org.hswebframework.ezorm.core.ValueCodec;

import java.util.Set;

@SuppressWarnings("all")
public interface ColumnMetaData extends ObjectMetaData, Cloneable {
    String getName();

    String getAlias();

    String getComment();

    Class getJavaType();

    <T extends TableMetaData> T getTableMetaData();

    ValueCodec getValueCodec();

    DictionaryCodec getDictionaryCodec();

    DefaultValue getDefaultValue();

    Set<String> getValidator();

    PropertyWrapper getProperty(String property);

    PropertyWrapper getProperty(String property, Object defaultValue);

    PropertyWrapper setProperty(String property, Object value);

}
