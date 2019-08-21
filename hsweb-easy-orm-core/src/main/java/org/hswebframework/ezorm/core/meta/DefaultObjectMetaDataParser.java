package org.hswebframework.ezorm.core.meta;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

public class DefaultObjectMetaDataParser implements ObjectMetaDataParser {

    private Map<String, ObjectMetaDataParserStrategy> strategies = new ConcurrentHashMap<>();

    protected void registerStrategy(ObjectMetaDataParserStrategy strategy) {
        strategies.put(strategy.getSupportType().getType(), strategy);
    }

    protected Optional<ObjectMetaDataParserStrategy> getStrategy(ObjectType type) {
        return Optional.ofNullable(strategies.get(type.getType()));
    }

    @Override
    public boolean objectExists(ObjectType type, String name) {
        return getStrategy(type)
                .map(strategy -> strategy.objectExists(name))
                .orElse(false);
    }

    @Override
    public Optional<ObjectMetaData> parse(ObjectType type, String name) {

        return getStrategy(type)
                .flatMap(strategy -> strategy.parse(name));
    }

    @Override
    public Set<String> getAllNames(ObjectType type) {
        return getStrategy(type)
                .map(ObjectMetaDataParserStrategy::getAllNames)
                .orElseGet(HashSet::new);
    }

    @Override
    public List<ObjectMetaData> parseAll(ObjectType type) {
        return getStrategy(type)
                .map(ObjectMetaDataParserStrategy::parseAll)
                .orElseGet(ArrayList::new);
    }
}
