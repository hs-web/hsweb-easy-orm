package org.hswebframework.ezorm.rdb.mapping.parser;

import org.hswebframework.ezorm.core.ValueCodec;

import java.beans.PropertyDescriptor;
import java.util.Optional;

public interface ValueCodecResolver {

    Optional<ValueCodec> resolve(Class entityType, PropertyDescriptor descriptor);

}
