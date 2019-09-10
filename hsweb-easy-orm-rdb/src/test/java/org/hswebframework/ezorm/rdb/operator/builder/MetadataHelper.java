package org.hswebframework.ezorm.rdb.operator.builder;

import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;

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
            id.setJdbcType(JDBCType.VARCHAR);
            id.setLength(32);

            RDBColumnMetadata name = new RDBColumnMetadata();
            name.setName("name");
            name.setJdbcType(JDBCType.VARCHAR);
            name.setLength(64);

            table.addColumn(id);
            table.addColumn(name);

            detail.addColumn(id.clone());
        }
        {

            RDBColumnMetadata comment = new RDBColumnMetadata();
            comment.setName("comment");
            comment.setJdbcType(JDBCType.VARCHAR);
            comment.setLength(64);

            detail.addColumn(comment);

        }


        return schema;
    }
}
