package org.hswebframework.ezorm.core.meta;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.CastUtil;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.Optional.*;
import static java.util.stream.Collectors.*;

public abstract class AbstractSchemaMetaData implements SchemaMetaData {

    private Map<String, Map<String, ObjectMetaData>> metaRepository = new ConcurrentHashMap<>();

    @Getter
    @Setter
    private ObjectMetaDataParser parser;

    @Getter
    @Setter
    private DatabaseMetaData database;

    @Getter
    @Setter
    private String name;

    @Override
    public abstract String getName();

    @Override
    public abstract List<ObjectType> getAllObjectType();

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
                    .collect(toMap(ObjectMetaData::getName, Function.identity(), (_1, _2) -> _1, ConcurrentHashMap::new));

            typeMapping = metaRepository.put(type.getType(), group);
            if (typeMapping != null) {
                typeMapping.forEach(group::putIfAbsent);
            }
            return all;
        }
        return (List) new ArrayList<>(typeMapping.values());
    }

    protected <T extends ObjectMetaData> List<T> parseMeta(ObjectType type) {
        return parser.parseAll(type);
    }

    protected <T extends ObjectMetaData> T parseMeta(ObjectType type, String name) {
        return parser
                .<T>parse(type, name)
                .orElse(null);
    }

    @Override
    public <T extends ObjectMetaData> Optional<T> getObject(ObjectType type, String name) {
        return ofNullable(metaRepository.get(type.getType()))
                .map(repo -> repo.computeIfAbsent(name, __ -> parseMeta(type, name)))
                .map(CastUtil::cast);
    }
}
