package org.hswebframework.ezorm.core.meta;

import org.hswebframework.ezorm.core.CastUtil;
import org.hswebframework.ezorm.core.FeatureType;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

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
