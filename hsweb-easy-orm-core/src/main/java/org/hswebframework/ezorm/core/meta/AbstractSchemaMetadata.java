package org.hswebframework.ezorm.core.meta;

import lombok.Getter;
import lombok.Setter;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.core.CastUtil;
import org.hswebframework.ezorm.core.FeatureId;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;

import static java.util.Optional.*;
import static java.util.stream.Collectors.*;

@Slf4j
public abstract class AbstractSchemaMetadata implements SchemaMetadata {

    private Map<String, Map<String, ObjectMetadata>> metaRepository = new ConcurrentHashMap<>();

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

            List<T> all = loadMetadata(type);
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

    protected <T extends ObjectMetadata> List<T> loadMetadata(ObjectType type) {
        return getParser(type)
                .map(ObjectMetadataParser::parseAll)
                .map(CastUtil::<List<T>>cast)
                .orElseGet(Collections::emptyList);
    }

    protected <T extends ObjectMetadata> T loadMetadata(ObjectType type, String name) {
        return getParser(type)
                .flatMap(parser -> {
                    log.debug("load {} metadata ,use parser:{}", type, parser.getClass().getSimpleName());
                    return parser.<T>parseByName(name);
                })
                .orElse(null);
    }

    protected Optional<ObjectMetadataParser> getParser(ObjectType type) {
        return getFeatures().values()
                .stream()
                .filter(ObjectMetadataParser.class::isInstance)
                .map(ObjectMetadataParser.class::cast)
                .filter(parser -> parser.getObjectType().getId().equals(type.getId()))
                .findFirst();
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
        Objects.requireNonNull(name, "name");
        return of(metaRepository.computeIfAbsent(type.getId(), t -> new ConcurrentHashMap<>()))
                .map(repo -> repo.computeIfAbsent(name, __ -> loadMetadata(type, name)))
                .map(CastUtil::cast);
    }

    public void addFeature(Feature feature) {
        features.put(feature.getId(), feature);
    }

    public <T extends Feature> Optional<T> findFeature(FeatureId<T> id) {
        return findFeature(id.getId());
    }

    public <T extends Feature> Optional<T> findFeature(String id) {
        return of(this.<T>getFeature(id))
                .filter(Optional::isPresent)
                .orElseGet(() -> Optional
                        .ofNullable(getDatabase())
                        .flatMap(database -> database.getFeature(id)));
    }

    @Override
    @SneakyThrows
    public AbstractSchemaMetadata clone() {
        AbstractSchemaMetadata schema = (AbstractSchemaMetadata) super.clone();
        schema.features = new HashMap<>(features);
        return schema;
    }
}
