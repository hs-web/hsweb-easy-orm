package org.hsweb.ezorm.core.meta;


import org.hsweb.ezorm.core.OptionConverter;
import org.hsweb.ezorm.core.PropertyWrapper;
import org.hsweb.ezorm.core.ValueConverter;

import java.io.Serializable;
import java.util.Set;

public interface ColumnMetaData extends Serializable, Cloneable {
    String getName();

    String getAlias();

    String getComment();

    Class getJavaType();

    <T extends TableMetaData> T getTableMetaData();

    ValueConverter getValueConverter();

    OptionConverter getOptionConverter();

    Set<String> getValidator();

    PropertyWrapper getProperty(String property);

    PropertyWrapper getProperty(String property, Object defaultValue);

    PropertyWrapper setProperty(String property, Object value);

    <T extends ColumnMetaData> T clone();
}
