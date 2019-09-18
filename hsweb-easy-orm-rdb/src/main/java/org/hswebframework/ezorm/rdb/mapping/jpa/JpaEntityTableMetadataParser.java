package org.hswebframework.ezorm.rdb.mapping.jpa;


import lombok.Setter;
import org.hswebframework.ezorm.rdb.mapping.EntityTableMetadataParser;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.utils.ClassUtils;

import javax.persistence.Table;
import java.util.Optional;

/**
 * @see javax.persistence.Column
 * @see javax.persistence.JoinTable
 * @see javax.persistence.CollectionTable
 */
public class JpaEntityTableMetadataParser implements EntityTableMetadataParser {

    @Setter
    private RDBDatabaseMetadata databaseMetadata;

    public Optional<RDBTableMetadata> parseTable(Class<?> entityType) {

        Table table = ClassUtils.getAnnotation(entityType, Table.class);
        if (table == null) {
            return Optional.empty();
        }
        RDBSchemaMetadata schema = databaseMetadata.getSchema(table.schema())
                .orElseGet(databaseMetadata::getCurrentSchema);


        RDBTableMetadata tableMetadata = schema.newTable(table.name());
        tableMetadata.setAlias(entityType.getSimpleName());

        new JpaEntityTableMetadataParserProcessor(tableMetadata,entityType).process();


        return Optional.of(tableMetadata);

    }




}
