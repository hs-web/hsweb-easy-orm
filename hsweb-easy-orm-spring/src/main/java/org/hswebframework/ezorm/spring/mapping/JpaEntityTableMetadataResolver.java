package org.hswebframework.ezorm.spring.mapping;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.mapping.jpa.JpaEntityTableMetadataParser;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.spring.EntityTableMetadataResolver;

@AllArgsConstructor
public class JpaEntityTableMetadataResolver implements EntityTableMetadataResolver {

    private DatabaseOperator databaseOperator;

    @Override
    public RDBTableMetadata resolve(Class<?> entityClass) {
        JpaEntityTableMetadataParser parser = new JpaEntityTableMetadataParser();
        parser.setDatabaseMetadata(databaseOperator.getMetadata());

        return parser.parseTable(entityClass).orElse(null);
    }


}
