package org.hswebframework.ezorm.rdb.mapping.jpa;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.rdb.codec.JsonValueCodec;
import org.hswebframework.ezorm.rdb.mapping.EntityPropertyDescriptor;
import org.hswebframework.ezorm.rdb.mapping.annotation.ColumnType;
import org.hswebframework.ezorm.rdb.mapping.annotation.JsonCodec;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.junit.Assert;
import org.junit.Test;

import javax.persistence.*;
import java.sql.JDBCType;
import java.sql.SQLType;
import java.util.Date;
import java.util.List;
import java.util.Map;

public class JpaEntityTableMetadataParserTest {

    @Test
    public void test() {

        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.H2);
        H2SchemaMetadata schema = new H2SchemaMetadata("PUBLIC");
        database.setCurrentSchema(schema);
        database.addSchema(schema);

        JpaEntityTableMetadataParser parser = new JpaEntityTableMetadataParser();
        parser.setDatabaseMetadata(database);

        RDBTableMetadata table = parser.parseTableMetadata(User.class).orElseThrow(IllegalArgumentException::new);

        Assert.assertEquals(table.getName(), "entity_test");

        RDBColumnMetadata id = table.getColumn("id").orElseThrow(NullPointerException::new);

        Assert.assertTrue(id.isPrimaryKey());

        Assert.assertEquals(id.getJavaType(), String.class);
        Assert.assertEquals(id.getLength(), 32);


        Assert.assertEquals(table.getColumn("create_time").orElseThrow(NullPointerException::new).getAlias(), "createTime");

        Assert.assertEquals(table.getColumn("aTest").orElseThrow(NullPointerException::new).getAlias(), "aTest");

    }

    @Test
    public void objectPropertyRetainsCustomDataTypeCodec() {
        assertCustomObjectCodec(ObjectEntity.class);
    }

    @Test
    public void objectPropertyWithGetterOnlyRetainsCustomDataTypeCodec() {
        assertCustomObjectCodec(ReadOnlyObjectEntity.class);
    }

    @Test
    public void objectPropertyWithSetterOnlyRetainsCustomDataTypeCodec() {
        assertCustomObjectCodec(WriteOnlyObjectEntity.class);
    }

    @Test
    public void explicitCodecAnnotationStillOverridesDataTypeCodec() {
        RDBColumnMetadata column = parseValueColumn(AnnotatedObjectEntity.class);
        Assert.assertEquals(Object.class, column.getFeatureNow(EntityPropertyDescriptor.ID).getPropertyType());
        Assert.assertTrue(column.getValueCodec() instanceof JsonValueCodec);
        assertJsonRoundTrip(column);
    }

    private static void assertCustomObjectCodec(Class<?> entityType) {
        RDBColumnMetadata column = parseValueColumn(entityType);
        Assert.assertEquals(Object.class, column.getJavaType());
        Assert.assertEquals(Object.class, column.getFeatureNow(EntityPropertyDescriptor.ID).getPropertyType());
        Assert.assertSame(column.getType(), column.getValueCodec());
        assertJsonRoundTrip(column);
    }

    private static void assertJsonRoundTrip(RDBColumnMetadata column) {
        Map<String, Object> value = Map.of("name", "JetLinks", "count", 3);
        Object encoded = column.getValueCodec().encode(value, column);
        Assert.assertTrue(encoded instanceof String);
        Assert.assertEquals(value, column.getValueCodec().decode(encoded, column));
    }

    private static RDBColumnMetadata parseValueColumn(Class<?> entityType) {
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.H2);
        H2SchemaMetadata schema = new H2SchemaMetadata("PUBLIC");
        database.setCurrentSchema(schema);
        database.addSchema(schema);
        JpaEntityTableMetadataParser parser = new JpaEntityTableMetadataParser();
        parser.setDatabaseMetadata(database);
        return parser.parseTableMetadata(entityType)
                     .orElseThrow(IllegalArgumentException::new)
                     .getColumn("value")
                     .orElseThrow(IllegalArgumentException::new);
    }

    public static class CustomJsonType implements DataType, ValueCodec<Object, Object> {

        private final JsonValueCodec codec = JsonValueCodec.of(Object.class);

        @Override
        public String getId() {
            return "custom-json";
        }

        @Override
        public String getName() {
            return "varchar";
        }

        @Override
        public SQLType getSqlType() {
            return JDBCType.VARCHAR;
        }

        @Override
        public Class<?> getJavaType() {
            return Object.class;
        }

        @Override
        public Object encode(Object value) {
            return codec.encode(value);
        }

        @Override
        public Object decode(Object value) {
            return codec.decode(value);
        }
    }

    public static class ObjectValue {
        @Column
        @ColumnType(type = CustomJsonType.class)
        protected Object value;
    }

    @Table(name = "object_value")
    public static class ObjectEntity extends ObjectValue {
        public Object getValue() {
            return value;
        }

        public void setValue(Object value) {
            this.value = value;
        }
    }

    @Table(name = "read_only_object_value")
    public static class ReadOnlyObjectEntity extends ObjectValue {
        public Object getValue() {
            return value;
        }
    }

    @Table(name = "write_only_object_value")
    public static class WriteOnlyObjectEntity extends ObjectValue {
        public void setValue(Object value) {
            this.value = value;
        }
    }

    @Getter
    @Setter
    @Table(name = "annotated_object_value")
    public static class AnnotatedObjectEntity {
        @Column
        @ColumnType(type = CustomJsonType.class)
        @JsonCodec
        private Object value;
    }

    public interface InterfaceEntity<ID> {
        ID getId();
    }

    @Getter
    @Setter
    public static class GenericEntity<ID> implements InterfaceEntity<ID> {

        @Id
        @Column(length = 32)
        private ID id;

        @ColumnType(typeId = "jsonb", jdbcType = JDBCType.VARCHAR)
        private List<String> jsonArray;

    }

    @Table(name = "address")
    @Getter
    @Setter
    public class Address {
        @Column
        @Id
        private String id;
        @Column
        private String name;
    }

    @Getter
    @Setter
    @Table(name = "entity_test",
            indexes = @Index(
                    name = "test_index",
                    columnList = "name asc,state desc"
            ))
    @ToString
    @EqualsAndHashCode(callSuper = true)
    public static class User extends GenericEntity<String> {

//        @Override
//        @Id
//        @Column
//        public String getId() {
//            return super.getId();
//        }

        @Column
        private String name;

        @Column
        private Byte state;

        @Column(name = "create_time")
        private Date createTime;

        @Column(table = "entity_detail", name = "name")
//    @JoinColumn(table = "entity_detail", name = "id", referencedColumnName = "id")
        private String infoName;

        @Column
        private String addressId;

        @Column
        private String aTest;

        @JoinColumn(name = "address_id")
        private Address address;

    }


}
