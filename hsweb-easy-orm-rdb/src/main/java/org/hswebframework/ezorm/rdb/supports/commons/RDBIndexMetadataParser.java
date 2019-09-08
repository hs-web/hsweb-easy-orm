package org.hswebframework.ezorm.rdb.supports.commons;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.meta.ObjectMetaDataParserStrategy;
import org.hswebframework.ezorm.core.meta.ObjectType;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBObjectType;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

public abstract class RDBIndexMetadataParser implements ObjectMetaDataParserStrategy<RDBIndexMetadata> {

    @Setter
    @Getter
    private String schema;

    @Override
    public ObjectType getSupportType() {
        return RDBObjectType.index;
    }

    @Override
    public abstract Optional<RDBIndexMetadata> parse(String name);

    @Override
    public abstract boolean objectExists(String name);

    @Override
    public abstract Set<String> getAllNames();

    @Override
    public List<RDBIndexMetadata> parseAll() {
        return getAllNames()
                .stream()
                .map(this::parse)
                .filter(Optional::isPresent)
                .map(Optional::get)
                .collect(Collectors.toList());
    }
}
