package org.hswebframework.ezorm.core.meta;

import lombok.Getter;
import lombok.Setter;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.core.CastUtil;
import org.hswebframework.ezorm.core.utils.StringUtils;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Function;
import java.util.function.Supplier;

import static java.util.Optional.*;
import static java.util.stream.Collectors.*;

@Slf4j
public abstract class AbstractSchemaMetadata implements SchemaMetadata {

    private final Map<String, Map<String, ObjectMetadata>> metaRepository = new ConcurrentHashMap<>();

    @Getter
    @Setter
    private DatabaseMetadata<?> database;

    @Getter
    @Setter
    private String name;

    @Getter
    @Setter
    private String alias;

    @Getter
    private Map<String, Feature> features = new ConcurrentHashMap<>();

    private final Lock lock = new ReentrantLock();

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
            Map<String, ObjectMetadata> group = all
                .stream()
                .collect(toMap(ObjectMetadata::getName, Function.identity(), (_1, _2) -> _1, ConcurrentHashMap::new));

            typeMapping = metaRepository.put(type.getId(), group);
            if (typeMapping != null) {
                typeMapping.forEach(group::putIfAbsent);
            }
            return all;
        }
        return (List) new ArrayList<>(typeMapping.values());
    }

    public <T extends ObjectMetadata> Flux<T> getObjectReactive(ObjectType type) {
        Map<String, ObjectMetadata> typeMapping = metaRepository.get(type.getId());
        if (typeMapping == null) {
            return loadMetadataReactive(type)
                .collectMap(ObjectMetadata::getName, Function.identity())
                .flatMapMany(group -> {
                    Map<String, ObjectMetadata> mapping = metaRepository.put(type.getId(), group);
                    if (mapping != null) {
                        mapping.forEach(group::putIfAbsent);
                    }
                    return Flux.fromIterable(group.values());
                })
                .map(CastUtil::cast);
        }
        return Flux.fromIterable(typeMapping.values()).map(CastUtil::cast);
    }

    protected <T extends ObjectMetadata> List<T> loadMetadata(ObjectType type) {
        return getParser(type)
            .map(ObjectMetadataParser::parseAll)
            .map(CastUtil::<List<T>>cast)
            .orElseGet(Collections::emptyList);
    }

    protected <T extends ObjectMetadata> T loadMetadata(ObjectType type, String name) {
        String fName = StringUtils.getPlainName(name);
        return getParser(type)
            .flatMap(parser -> {
                log.debug("load {} metadata {} ,use parser:{}", type, fName, parser.getClass().getSimpleName());
                return parser.parseByName(fName);
            })
            .map(CastUtil::<T>cast)
            .orElse(null);
    }

    protected <T extends ObjectMetadata> Flux<T> loadMetadataReactive(ObjectType type) {
        return Mono.justOrEmpty(getParser(type))
                   .flatMapMany(ObjectMetadataParser::parseAllReactive)
                   .map(CastUtil::cast);
    }

    protected <T extends ObjectMetadata> Mono<T> loadMetadataReactive(ObjectType type, String name) {
        String fName = StringUtils.getPlainName(name);
        return Mono.justOrEmpty(getParser(type))
                   .flatMap(parser -> {
                       log.debug("reactive load {} [{}] metadata ,use parser:{}", type, fName, parser
                           .getClass()
                           .getSimpleName());
                       return parser.parseByNameReactive(fName);
                   })
                   .map(CastUtil::cast);
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
        Map<String, ObjectMetadata> repo = metaRepository
            .computeIfAbsent(metadata
                                 .getObjectType()
                                 .getId(), t -> new ConcurrentHashMap<>());

        repo.put(metadata.getName(), metadata);
        if (metadata.getAlias() != null) {
            repo.put(metadata.getAlias(), metadata);
        }
    }

    @Override
    public <T extends ObjectMetadata> Optional<T> removeObject(ObjectType type, String name) {
        Objects.requireNonNull(name, "name");
        return ofNullable(metaRepository.get(type.getId()))
            .map(repo -> repo.remove(StringUtils.getPlainName(name)))
            .map(CastUtil::cast);
    }

    public <T extends ObjectMetadata> Mono<T> getObjectReactive(ObjectType type, String name, boolean autoLoad) {
        Objects.requireNonNull(name, "name");
        String fname = StringUtils.getPlainName(name);
        Map<String, ObjectMetadata> mapping = metaRepository.computeIfAbsent(type.getId(), t -> new ConcurrentHashMap<>());

        ObjectMetadata metadata = getLoadedObject0(mapping, fname);
        if (metadata == null && autoLoad) {
            return loadMetadataReactive(type, fname)
                .doOnNext(obj -> mapping.put(fname, obj))
                .map(CastUtil::cast);
        }
        return Mono.justOrEmpty(CastUtil.<T>cast(metadata));
    }

    protected ObjectMetadata getLoadedObject0(Map<String, ObjectMetadata> from, String name) {
        ObjectMetadata metadata = from.get(name);
        if (metadata == null) {
            metadata = from.get(name.toLowerCase());
        }
        return metadata;
    }

    public <T extends ObjectMetadata> Mono<T> getObjectReactive(ObjectType type, String name) {
        return getObjectReactive(type, name, true);
    }

    @Override
    public <T extends ObjectMetadata> Optional<T> getObject(ObjectType type, String name) {
        return getObject(type, name, false);
    }

    @Override
    public <T extends ObjectMetadata> Optional<T> getObject(ObjectType type, String name, boolean autoLoad) {
        Objects.requireNonNull(name, "name");
        name = StringUtils.getPlainName(name);
        Map<String, ObjectMetadata> mapping = metaRepository.computeIfAbsent(type.getId(), t -> new ConcurrentHashMap<>());

        ObjectMetadata metadata = getLoadedObject0(mapping, name);
        if (metadata == null && autoLoad) {
            try {
                lock.lock();
                metadata = getLoadedObject0(mapping, name);
                if (metadata != null) {
                    return Optional.of(CastUtil.cast(metadata));
                }
                metadata = loadMetadata(type, name);
                if (null != metadata) {
                    mapping.put(name, metadata);
                }
            }finally {
                lock.unlock();
            }
        }
        return Optional.ofNullable(CastUtil.cast(metadata));
    }

    public void addFeature(Feature feature) {
        features.put(feature.getId(), feature);
    }

    @Override
    public <T extends Feature> T findFeatureOrElse(String id, Supplier<T> orElse) {
        T current = getFeatureOrElse(id, null);
        if (null != current) {
            return current;
        }
        DatabaseMetadata<?> db = getDatabase();
        if (db != null) {
            return db.findFeatureOrElse(id, orElse);
        }
        return orElse == null ? null : orElse.get();
    }

    @Override
    @SneakyThrows
    public AbstractSchemaMetadata clone() {
        AbstractSchemaMetadata schema = (AbstractSchemaMetadata) super.clone();
        schema.features = new ConcurrentHashMap<>(features);
        return schema;
    }
}
