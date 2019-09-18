package org.hswebframework.ezorm.rdb.mapping.jpa;

import org.hswebframework.ezorm.rdb.mapping.TestEntity;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.junit.Assert;
import org.junit.Test;

import static org.junit.Assert.*;

public class JpaEntityTableMetadataParserTest {

    @Test
    public void test() {

        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.H2);
        H2SchemaMetadata schema = new H2SchemaMetadata("PUBLIC");
        database.setCurrentSchema(schema);
        database.addSchema(schema);

        JpaEntityTableMetadataParser parser = new JpaEntityTableMetadataParser();
        parser.setDatabaseMetadata(database);

        RDBTableMetadata table = parser.parseTable(TestEntity.class).orElseThrow(IllegalArgumentException::new);

        Assert.assertEquals(table.getName(),"entity_test");

        Assert.assertTrue(table.getColumn("id").orElseThrow(NullPointerException::new).isPrimaryKey());

        Assert.assertEquals(table.getColumn("create_time").orElseThrow(NullPointerException::new).getAlias(),"createTime");

    }

}