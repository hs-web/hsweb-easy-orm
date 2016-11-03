package org.hsweb.ezorm.core.meta;

import org.hsweb.ezorm.core.ObjectWrapper;
import org.hsweb.ezorm.core.PropertyWrapper;
import org.hsweb.ezorm.core.Trigger;

import java.io.Serializable;
import java.util.Map;
import java.util.Set;

/**
 * @author zhouhao
 */
public interface TableMetaData extends Serializable {
    String getName();

    String getComment();

    String getAlias();

    <T extends DatabaseMetaData> T getDatabaseMetaData();

    <T extends ColumnMetaData> Set<T> getColumns();

    <T extends ColumnMetaData> T getColumn(String name);

    <T extends ColumnMetaData> T findColumn(String name);

    <T> ObjectWrapper<T> getObjectWrapper();

    PropertyWrapper getProperty(String property);

    PropertyWrapper getProperty(String property, Object defaultValue);

    PropertyWrapper setProperty(String property, Object value);

    void on(String name, Trigger trigger);

    void on(String name, Map<String, Object> triggerContext);

    boolean triggerIsSupport(String name);
}
