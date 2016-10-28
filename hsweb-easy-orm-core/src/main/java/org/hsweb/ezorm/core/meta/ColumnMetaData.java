package org.hsweb.ezorm.core.meta;


import org.hsweb.ezorm.core.OptionConverter;
import org.hsweb.ezorm.core.PropertyWrapper;
import org.hsweb.ezorm.core.ValueConverter;

public interface ColumnMetaData {
    String getName();

    String getAlias();

    String getComment();

    Class getJavaType();

    <T extends TableMetaData> T getTableMetaData();

    ValueConverter getValueConverter();

    OptionConverter getOptionConverter();

    PropertyWrapper getProperty(String property);

    PropertyWrapper getProperty(String property, Object defaultValue);

    PropertyWrapper setProperty(String property, Object value);

}
