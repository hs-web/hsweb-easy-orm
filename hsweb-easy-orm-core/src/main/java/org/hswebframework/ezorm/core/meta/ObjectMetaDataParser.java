package org.hswebframework.ezorm.core.meta;


import java.util.List;
import java.util.Optional;
import java.util.Set;

public interface ObjectMetaDataParser {

    boolean objectExists(ObjectType type, String name);

    Optional<ObjectMetaData> parse(ObjectType type, String name);

    Set<String> getAllNames(ObjectType type);

    List<ObjectMetaData> parseAll(ObjectType type);


}
