package org.hswebframework.ezorm.core.meta;


import java.util.List;
import java.util.Optional;
import java.util.Set;

public interface ObjectMetaDataParser {

    boolean objectExists(ObjectType type, String name);

    <T extends ObjectMetaData> Optional<T> parse(ObjectType type, String name);

    Set<String> getAllNames(ObjectType type);

    <T extends ObjectMetaData> List<T> parseAll(ObjectType type);

}
