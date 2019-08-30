package org.hswebframework.ezorm.core.meta;

/**
 * @author zhouhao
 * @since 4.0.0
 */
public interface ObjectMetadata {

    String getName();

    String getAlias();

    ObjectType getObjectType();

    default boolean equalsNameOrAlias(String nameOrAlias) {
        return nameOrAlias != null
                && (nameOrAlias.equalsIgnoreCase(getName()) || nameOrAlias.equalsIgnoreCase(getAlias())
        );
    }
}
