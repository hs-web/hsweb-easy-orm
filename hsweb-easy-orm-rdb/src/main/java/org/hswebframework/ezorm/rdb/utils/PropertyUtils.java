package org.hswebframework.ezorm.rdb.utils;

import org.hswebframework.ezorm.core.Extensible;
import org.hswebframework.ezorm.core.GlobalConfig;
import org.hswebframework.ezorm.core.ObjectPropertyOperator;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.EntityPropertyDescriptor;

import java.util.Optional;

public class PropertyUtils {

    public static Optional<Object> getProperty(Object entity, String property, EntityColumnMapping mapping) {
        ObjectPropertyOperator opt = GlobalConfig.getPropertyOperator();
        if (entity instanceof Extensible && isExtensibleColumn(property, mapping)) {
            return Optional.ofNullable(((Extensible) entity).getExtension(property));
        }
        return opt.getProperty(entity, property);
    }

    public static boolean isExtensibleColumn(String property, EntityColumnMapping mapping) {
        return mapping
            .getColumnByProperty(property)
            .map(c -> !c.getFeature(EntityPropertyDescriptor.ID).isPresent())
            .orElse(true);
    }

    public static void setProperty(Object entity, String property, Object value, EntityColumnMapping mapping) {
        if (entity instanceof Extensible && isExtensibleColumn(property, mapping)) {
            ((Extensible) entity).setExtension(property, value);
        } else {
            GlobalConfig.getPropertyOperator().setProperty(entity, property, value);
        }
    }
}