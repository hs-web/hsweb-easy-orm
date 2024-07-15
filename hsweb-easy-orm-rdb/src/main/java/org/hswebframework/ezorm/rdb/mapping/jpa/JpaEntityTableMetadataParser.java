package org.hswebframework.ezorm.rdb.mapping.jpa;


import lombok.Setter;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.mapping.annotation.Comment;
import org.hswebframework.ezorm.rdb.mapping.parser.*;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.utils.AnnotationUtils;
import org.hswebframework.utils.ClassUtils;

import javax.persistence.Table;
import java.util.Optional;
import java.util.concurrent.Callable;
import java.util.function.Supplier;

/**
 * @see javax.persistence.Column
 * @see javax.persistence.JoinTable
 * @see javax.persistence.JoinColumn
 */
public class JpaEntityTableMetadataParser implements EntityTableMetadataParser {

    private Callable<RDBDatabaseMetadata> databaseMetadata;

    public void setDatabaseMetadata(Callable<RDBDatabaseMetadata> databaseMetadata) {
        this.databaseMetadata = databaseMetadata;
    }

    public void setDatabaseMetadata(RDBDatabaseMetadata databaseMetadata) {
        this.databaseMetadata = () -> databaseMetadata;
    }

    @Setter
    private DataTypeResolver dataTypeResolver = DefaultDataTypeResolver.INSTANCE;

    @Setter
    private ValueCodecResolver valueCodecResolver = DefaultValueCodecResolver.COMMONS;


    @SneakyThrows
    public Optional<RDBTableMetadata> parseTableMetadata(Class<?> entityType) {

        Table table = AnnotationUtils.getAnnotation(entityType, Table.class);
        if (table == null) {
            return Optional.empty();
        }
        RDBDatabaseMetadata databaseMetadata = this.databaseMetadata.call();
        RDBSchemaMetadata schema = databaseMetadata
                .getSchema(table.schema())
                .orElseGet(databaseMetadata::getCurrentSchema);


        RDBTableMetadata tableMetadata = schema.newTable(table.name());
        //tableMetadata.setAlias(entityType.getSimpleName());

        Optional.ofNullable(ClassUtils.getAnnotation(entityType, Comment.class))
                .map(Comment::value)
                .ifPresent(tableMetadata::setComment);

        JpaEntityTableMetadataParserProcessor parserProcessor = createProcessor(tableMetadata, entityType);
        parserProcessor.setDataTypeResolver(dataTypeResolver);
        parserProcessor.setValueCodecResolver(valueCodecResolver);
        parserProcessor.process();


        return Optional.of(tableMetadata);
    }

    protected JpaEntityTableMetadataParserProcessor createProcessor(RDBTableMetadata table, Class<?> type) {
        return new JpaEntityTableMetadataParserProcessor(table, type);
    }

}
