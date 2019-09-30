package org.hswebframework.ezorm.rdb.mapping.jpa;


import lombok.Setter;
import org.hswebframework.ezorm.rdb.mapping.annotation.Comment;
import org.hswebframework.ezorm.rdb.mapping.parser.*;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.utils.AnnotationUtils;
import org.hswebframework.utils.ClassUtils;

import javax.persistence.Table;
import java.util.Optional;

/**
 * @see javax.persistence.Column
 * @see javax.persistence.JoinTable
 * @see javax.persistence.JoinColumn
 */
public class JpaEntityTableMetadataParser implements EntityTableMetadataParser {

    @Setter
    private RDBDatabaseMetadata databaseMetadata;

    @Setter
    private DataTypeResolver dataTypeResolver = DefaultDataTypeResolver.INSTANCE;

    @Setter
    private ValueCodecResolver valueCodecResolver = DefaultValueCodecResolver.COMMONS;


    public Optional<RDBTableMetadata> parseTableMetadata(Class<?> entityType) {

        Table table = AnnotationUtils.getAnnotation(entityType, Table.class);
        if (table == null) {
            return Optional.empty();
        }
        RDBSchemaMetadata schema = databaseMetadata.getSchema(table.schema())
                .orElseGet(databaseMetadata::getCurrentSchema);


        RDBTableMetadata tableMetadata = schema.newTable(table.name());
        //tableMetadata.setAlias(entityType.getSimpleName());

        Optional.ofNullable(ClassUtils.getAnnotation(entityType, Comment.class))
                .map(Comment::value)
                .ifPresent(tableMetadata::setComment);

        JpaEntityTableMetadataParserProcessor parserProcessor = new JpaEntityTableMetadataParserProcessor(tableMetadata, entityType);
        parserProcessor.setDataTypeResolver(dataTypeResolver);
        parserProcessor.setValueCodecResolver(valueCodecResolver);
        parserProcessor.process();


        return Optional.of(tableMetadata);

    }


}
