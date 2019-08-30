package org.hswebframework.ezorm.core.meta;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.CastUtil;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
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

    @Getter
    private Map<String, Feature> features = new HashMap<>();

    @Override
    public abstract List<ObjectType> getAllObjectType();

    @Override
    public ObjectType getObjectType() {
        return DefaultObjectType.schema;
    }

    @Override
    @SuppressWarnings("all")
    public <T extends ObjectMetadata> List<T> getObject(ObjectType type) {
        Map<String, ObjectMetadata> typeMapping = metaRepository.get(type.getId());
        if (typeMapping == null) {

            List<T> all = parseMeta(type);
            Map<String, ObjectMetadata> group = all.stream()
                    .collect(toMap(ObjectMetadata::getName, Function.identity(), (_1, _2) -> _1, ConcurrentHashMap::new));

            typeMapping = metaRepository.put(type.getId(), group);
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
        return ofNullable(parser)
                .flatMap(_parser -> _parser.<T>parse(type, name))
                .orElse(null);
    }

    public void addObject(ObjectMetadata metadata) {
        Map<String, ObjectMetadata> repo = metaRepository.computeIfAbsent(metadata.getObjectType().getId(), t -> new ConcurrentHashMap<>());

        repo.put(metadata.getName(), metadata);
        if (metadata.getAlias() != null) {
            repo.put(metadata.getAlias(), metadata);
        }
    }

    @Override
    public <T extends ObjectMetadata> Optional<T> getObject(ObjectType type, String name) {
        return of(metaRepository.computeIfAbsent(type.getId(), t -> new ConcurrentHashMap<>()))
                .map(repo -> repo.computeIfAbsent(name, __ -> parseMeta(type, name)))
                .map(CastUtil::cast);
    }

    public void addFeature(Feature feature) {
        features.put(feature.getId(), feature);
    }
}
