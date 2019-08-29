package org.hswebframework.ezorm.core.meta;

import java.util.List;
import java.util.Optional;
import java.util.Set;

public interface ObjectMetaDataParserStrategy<T extends ObjectMetadata> {

    ObjectType getSupportType();

    Optional<T> parse(String name);

    boolean objectExists(String name);

    Set<String> getAllNames();

    List<T> parseAll();

}
