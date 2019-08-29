package org.hswebframework.ezorm.core.meta;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.CastUtil;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.function.Function;

import static java.util.Optional.*;
import static java.util.stream.Collectors.*;

public abstract class AbstractSchemaMetadata implements SchemaMetadata {

    private Map<String, Map<String, ObjectMetadata>> metaRepository = new ConcurrentHashMap<>();

    @Getter
    @Setter
    private ObjectMetaDataParser parser;

    @Getter
    @Setter
    private DatabaseMetadata database;

    @Getter
    @Setter
    private String name;

    @Getter
    @Setter
    private String alias;

    @Override
    public abstract List<ObjectType> getAllObjectType();

    @Override
    public ObjectType getObjectType() {
        return DefaultObjectType.schema;
    }

    @Override
    @SuppressWarnings("all")
    public <T extends ObjectMetadata> List<T> getObject(ObjectType type) {
        Map<String, ObjectMetadata> typeMapping = metaRepository.get(type.getType());
        if (typeMapping == null) {

            List<T> all = parseMeta(type);
            Map<String, ObjectMetadata> group = all.stream()
                    .collect(toMap(ObjectMetadata::getName, Function.identity(), (_1, _2) -> _1, ConcurrentHashMap::new));

            typeMapping = metaRepository.put(type.getType(), group);
            if (typeMapping != null) {
                typeMapping.forEach(group::putIfAbsent);
            }
            return all;
        }
        return (List) new ArrayList<>(typeMapping.values());
    }

    protected <T extends ObjectMetadata> List<T> parseMeta(ObjectType type) {
        return parser.parseAll(type);
    }

    protected <T extends ObjectMetadata> T parseMeta(ObjectType type, String name) {
        return parser
                .<T>parse(type, name)
                .orElse(null);
    }

    @Override
    public <T extends ObjectMetadata> Optional<T> getObject(ObjectType type, String name) {
        return of(metaRepository.computeIfAbsent(type.getType(), t -> new ConcurrentHashMap<>()))
                .map(repo -> repo.computeIfAbsent(name, __ -> parseMeta(type, name)))
                .map(CastUtil::cast);
    }
}
