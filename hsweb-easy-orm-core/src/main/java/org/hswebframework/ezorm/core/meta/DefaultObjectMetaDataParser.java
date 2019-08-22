package org.hswebframework.ezorm.core.meta;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

public class DefaultObjectMetaDataParser implements ObjectMetaDataParser {

    private Map<String, ObjectMetaDataParserStrategy> strategies = new ConcurrentHashMap<>();

    protected void registerStrategy(ObjectMetaDataParserStrategy strategy) {
        strategies.put(strategy.getSupportType().getType(), strategy);
    }

    @SuppressWarnings("all")
    protected <T extends ObjectMetaData> Optional<ObjectMetaDataParserStrategy<T>> getStrategy(ObjectType type) {
        return Optional.ofNullable(strategies.get(type.getType()));
    }

    @Override
    public boolean objectExists(ObjectType type, String name) {
        return getStrategy(type)
                .map(strategy -> strategy.objectExists(name))
                .orElse(false);
    }

    @Override
    public <T extends ObjectMetaData> Optional<T> parse(ObjectType type, String name) {

        return this.<T>getStrategy(type)
                .flatMap(strategy -> strategy.parse(name));
    }

    @Override
    public Set<String> getAllNames(ObjectType type) {
        return getStrategy(type)
                .map(ObjectMetaDataParserStrategy::getAllNames)
                .orElseGet(HashSet::new);
    }

    @Override
    public <T extends ObjectMetaData> List<T> parseAll(ObjectType type) {
        return this.<T>getStrategy(type)
                .map(ObjectMetaDataParserStrategy::parseAll)
                .orElseGet(ArrayList::new);
    }
}
