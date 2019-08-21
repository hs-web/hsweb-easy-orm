package org.hswebframework.ezorm.core.meta;

import org.hswebframework.ezorm.core.CastUtil;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.Optional.*;

public abstract class AbstractSchemaMetaData implements SchemaMetaData {

    private Map<String, Map<String, ObjectMetaData>> metaRepository = new ConcurrentHashMap<>();

    @Override
    public abstract String getName();

    @Override
    public List<ObjectType> getAllObjectType() {
        return null;
    }

    @Override
    public ObjectType getObjectType() {
        return DefaultObjectType.schema;
    }

    @Override
    @SuppressWarnings("all")
    public <T extends ObjectMetaData> List<T> getObject(ObjectType type) {
        Map<String, ObjectMetaData> typeMapping = metaRepository.get(type.getType());
        if (typeMapping == null) {

            List<T> all = parseMeta(type);
            Map<String, ObjectMetaData> group = all.stream()
                    .collect(Collectors.toMap(ObjectMetaData::getName, Function.identity(), (_1, _2) -> _1, ConcurrentHashMap::new));

            typeMapping = metaRepository.put(type.getType(), group);
            if (typeMapping != null) {
                typeMapping.forEach(group::putIfAbsent);
            }
            return all;
        }
        return (List) new ArrayList<>(typeMapping.values());
    }

    protected abstract <T extends ObjectMetaData> List<T> parseMeta(ObjectType type);

    protected abstract <T extends ObjectMetaData> T parseMeta(ObjectType type, String name);

    @Override
    public <T extends ObjectMetaData> Optional<T> getObject(ObjectType type, String name) {

        return ofNullable(metaRepository.get(type.getType()))
                .map(repo -> repo.computeIfAbsent(name, parseMeta(type, name)))
                .map(CastUtil::cast);
    }
}
