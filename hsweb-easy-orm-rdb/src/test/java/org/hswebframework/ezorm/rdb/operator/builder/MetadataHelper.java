package org.hswebframework.ezorm.rdb.operator.builder;

import org.hswebframework.ezorm.rdb.metadata.*;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;

import java.sql.JDBCType;

public class MetadataHelper {


    public static RDBSchemaMetadata createMockSchema() {
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.H2);
        RDBSchemaMetadata schema = new RDBSchemaMetadata("PUBLIC");

        database.setCurrentSchema(schema);
        database.addSchema(schema);

        RDBTableMetadata table = new RDBTableMetadata();
        table.setName("test");

        RDBTableMetadata detail = new RDBTableMetadata();
        detail.setName("detail");

        schema.addTable(table);
        schema.addTable(detail);

        {
            RDBColumnMetadata id = new RDBColumnMetadata();
            id.setName("id");
            id.setType(JdbcDataType.of(JDBCType.VARCHAR,String.class));
            id.setLength(32);

            RDBColumnMetadata name = new RDBColumnMetadata();
            name.setName("name");
            name.setType(JdbcDataType.of(JDBCType.VARCHAR,String.class));
            name.setLength(64);

            table.addColumn(id);
            table.addColumn(name);

            detail.addColumn(id.clone());
        }
        {

            RDBColumnMetadata comment = new RDBColumnMetadata();
            comment.setName("comment");
            comment.setType(JdbcDataType.of(JDBCType.VARCHAR,String.class));
            comment.setLength(64);

            detail.addColumn(comment);

        }


        return schema;
    }
}
