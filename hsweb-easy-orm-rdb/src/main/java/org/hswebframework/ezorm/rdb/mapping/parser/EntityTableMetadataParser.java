package org.hswebframework.ezorm.rdb.mapping.parser;


import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;

import java.util.Collection;
import java.util.Optional;

public interface EntityTableMetadataParser {

    Optional<RDBTableMetadata> parseTableMetadata(Class<?> entityType);

}
