package org.hswebframework.ezorm.core.meta;

import org.hswebframework.ezorm.core.DictionaryCodec;
import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.core.FeatureType;
import org.hswebframework.ezorm.core.ValueCodec;
import org.junit.Assert;
import org.junit.Test;
import reactor.core.publisher.Mono;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

public class MetadataSupportTest {

    @Test
    public void testFeatureLookupAndFallback() {
        TestDatabase database = new TestDatabase("db");
        TestSchema schema = new TestSchema("schema");
        database.addSchema(schema);
        schema.setDatabase(database);

        TestFeature schemaFeature = new TestFeature("schemaFeature", Type.metadata);
        TestFeature databaseFeature = new TestFeature("databaseFeature", Type.metadata);
        schema.addFeature(schemaFeature);
        database.addFeature(databaseFeature);

        Assert.assertEquals(1, schema.getFeatureList().size());
        Assert.assertSame(schemaFeature, schema.getFeature(Type.metadata).orElse(null));
        Assert.assertSame(schemaFeature, schema.getFeatures(Type.metadata).get(0));
        Assert.assertSame(schemaFeature, schema.getFeatureNow(FeatureId.of("schemaFeature")));
        Assert.assertSame(databaseFeature, schema.findFeatureNow("databaseFeature"));
        Assert.assertTrue(schema.supportFeature("schemaFeature"));
        Assert.assertTrue(schema.supportFeature(schemaFeature));
        Assert.assertEquals("fallback", schema.findFeatureOrElse("missing", () -> new TestFeature("fallback", Type.other)).getId());

        try {
            schema.getFeatureNow("missing");
            Assert.fail("missing feature should fail");
        } catch (UnsupportedOperationException e) {
            Assert.assertTrue(e.getMessage().contains("missing"));
        }
    }

    @Test
    public void testDatabaseSchemaAndObjectLookup() {
        TestDatabase database = new TestDatabase("db");
        TestSchema publicSchema = new TestSchema("public");
        publicSchema.setAlias("pub");
        database.setCurrentSchema(publicSchema);
        database.addSchema(publicSchema);

        TestObject table = new TestObject("device", "dev", Type.object);
        publicSchema.addObject(table);

        Assert.assertSame(publicSchema, database.getSchema("public").orElse(null));
        Assert.assertSame(publicSchema, database.getSchema("pub").orElse(null));
        Assert.assertEquals(2, database.getSchemas().size());
        Assert.assertSame(table, database.getObject("device", (schema, name) -> schema.getObject(Type.object, name)).orElse(null));
        Assert.assertSame(table, database.getObject("public.device", (schema, name) -> schema.getObject(Type.object, name)).orElse(null));
        Assert.assertFalse(database.getObject(null, (schema, name) -> Optional.empty()).isPresent());

        Assert.assertSame(table, database.getObjectReactive("public.device",
                                                           (schema, name) -> schema.getObjectReactive(Type.object, name))
                                         .block());
        Assert.assertNull(database.getObjectReactive(null, (schema, name) -> Mono.empty()).block());

        AbstractDatabaseMetadata<TestSchema> cloned = database.clone();
        Assert.assertNotSame(database, cloned);
        Assert.assertSame(database.getCurrentSchema(), cloned.getCurrentSchema());
        Assert.assertNotSame(database.getSchema("public").orElse(null), cloned.getSchema("public").orElse(null));
        Assert.assertSame(table, publicSchema.removeObject(Type.object, "device").orElse(null));
        Assert.assertFalse(publicSchema.getObject(Type.object, "device").isPresent());
    }



    @Test
    public void testSchemaParserAutoLoadAndCacheBehavior() {
        TestSchema schema = new TestSchema("public");
        ParserFeature parser = new ParserFeature();
        ParserFeature otherParser = new OtherParserFeature();
        schema.addFeature(parser);
        schema.addFeature(otherParser);

        Assert.assertEquals(2, schema.getObject(Type.object).size());
        Assert.assertEquals(1, parser.parseAllCount);
        Assert.assertEquals(2, schema.getObject(Type.object).size());
        Assert.assertEquals("deviceA", schema.getObject(Type.object, "deviceA").orElseThrow().getName());
        Assert.assertEquals("deviceB", schema.getObject(Type.object, "deviceB").orElseThrow().getName());
        Assert.assertFalse(schema.getObject(Type.object, "unknown").isPresent());

        Assert.assertFalse(schema.getObject(Type.other, "remote", false).isPresent());
        Assert.assertEquals("remote", schema.getObject(Type.other, "`remote`", true).orElseThrow().getName());
        Assert.assertEquals(1, otherParser.parseByNameCount);
        Assert.assertEquals("remote", schema.getObject(Type.other, "remote", true).orElseThrow().getName());
        Assert.assertEquals(1, otherParser.parseByNameCount);

        Assert.assertEquals("reactive", schema.getObjectReactive(Type.other, "\"reactive\"", true).block().getName());
        Assert.assertEquals(1, otherParser.parseByNameReactiveCount);
        Assert.assertEquals("reactive", schema.getObjectReactive(Type.other, "reactive", false).block().getName());
        Assert.assertEquals(1, otherParser.parseByNameReactiveCount);

        schema.removeObject(Type.object, "deviceA");
        Assert.assertFalse(schema.getObject(Type.object, "deviceA").isPresent());
    }

    @Test
    public void testSchemaReactiveLoadAllAndCloneFeatures() {
        TestSchema schema = new TestSchema("public");
        ParserFeature parser = new ParserFeature();
        schema.addFeature(parser);

        Assert.assertEquals(2, schema.getObjectReactive(Type.object).collectList().block().size());
        Assert.assertEquals(1, parser.parseAllReactiveCount);
        Assert.assertEquals(2, schema.getObjectReactive(Type.object).collectList().block().size());
        Assert.assertEquals(1, parser.parseAllReactiveCount);

        AbstractSchemaMetadata cloned = schema.clone();
        Assert.assertNotSame(schema, cloned);
        Assert.assertSame(parser, cloned.getFeatureNow("parser"));
        schema.addFeature(new TestFeature("newFeature", Type.other));
        Assert.assertFalse(cloned.getFeature("newFeature").isPresent());
    }

    @Test
    public void testColumnMetadataEncodeDecodeAndProperties() {
        TestColumn column = new TestColumn("name");
        column.setAlias(null);
        column.setRealName("real_name");
        column.setValueCodec(new PrefixCodec());
        column.setDictionaryCodec(new SimpleDictionaryCodec());

        Assert.assertEquals("name", column.getAlias());
        Assert.assertEquals("real_name", column.getRealName());
        Assert.assertTrue(column.realNameDetected());
        Assert.assertEquals("dict:encoded:value", column.encode("value"));
        Assert.assertEquals("dict:decoded:value", column.decode("encoded:value"));
        Assert.assertEquals("encoded:null", column.encode(null));

        Assert.assertNull(column.getProperty("missing").getValue());
        Assert.assertEquals("default", column.getProperty("missing", "default").getValue());
        Assert.assertNull(column.setProperty("k", "v").getValue());
        Assert.assertEquals("v", column.getProperty("k").getValue());

        TestFeature feature = new TestFeature("columnFeature", Type.other);
        column.addFeature(feature);
        Assert.assertSame(feature, column.getFeatureNow("columnFeature"));
        Assert.assertEquals(DefaultObjectType.schema, new TestSchema("schema").getObjectType());
        Assert.assertTrue(column.equalsNameOrAlias("NAME"));
        Assert.assertFalse(column.equalsNameOrAlias(null));
    }

    enum Type implements ObjectType, FeatureType {
        object("对象"),
        metadata("元数据"),
        other("其他");

        private final String name;

        Type(String name) {
            this.name = name;
        }

        @Override
        public String getId() {
            return name();
        }

        @Override
        public String getName() {
            return name;
        }
    }

    static class TestFeature implements Feature {
        private final String id;
        private final FeatureType type;

        TestFeature(String id, FeatureType type) {
            this.id = id;
            this.type = type;
        }

        @Override
        public String getId() {
            return id;
        }

        @Override
        public String getName() {
            return id;
        }

        @Override
        public FeatureType getType() {
            return type;
        }
    }



    static class OtherParserFeature extends ParserFeature {
        OtherParserFeature() {
            super("otherParser");
        }

        @Override
        public ObjectType getObjectType() {
            return Type.other;
        }
    }

    static class ParserFeature extends TestFeature implements ObjectMetadataParser {
        int parseAllCount;
        int parseByNameCount;
        int parseAllReactiveCount;
        int parseByNameReactiveCount;

        ParserFeature() {
            this("parser");
        }

        ParserFeature(String id) {
            super(id, DefaultFeatureType.metadataParser);
        }

        @Override
        public ObjectType getObjectType() {
            return Type.object;
        }

        @Override
        public Optional<? extends ObjectMetadata> parseByName(String name) {
            parseByNameCount++;
            return Optional.of(new TestObject(name, name + "Alias", Type.other));
        }

        @Override
        public List<? extends ObjectMetadata> parseAll() {
            parseAllCount++;
            return java.util.Arrays.asList(new TestObject("deviceA", null, Type.object),
                                           new TestObject("deviceB", null, Type.object));
        }

        @Override
        public Mono<? extends ObjectMetadata> parseByNameReactive(String name) {
            parseByNameReactiveCount++;
            return Mono.just(new TestObject(name, name + "Alias", Type.other));
        }

        @Override
        public reactor.core.publisher.Flux<? extends ObjectMetadata> parseAllReactive() {
            parseAllReactiveCount++;
            return reactor.core.publisher.Flux.fromIterable(parseAll());
        }
    }

    static class TestDatabase extends AbstractDatabaseMetadata<TestSchema> {
        TestDatabase(String name) {
            setName(name);
        }
    }

    static class TestSchema extends AbstractSchemaMetadata {
        TestSchema(String name) {
            setName(name);
        }

        @Override
        public List<ObjectType> getAllObjectType() {
            return Collections.singletonList(Type.object);
        }
    }

    static class TestObject implements ObjectMetadata {
        private final String name;
        private final String alias;
        private final ObjectType objectType;

        TestObject(String name, String alias, ObjectType objectType) {
            this.name = name;
            this.alias = alias;
            this.objectType = objectType;
        }

        @Override
        public String getName() {
            return name;
        }

        @Override
        public String getAlias() {
            return alias;
        }

        @Override
        public ObjectType getObjectType() {
            return objectType;
        }

        @Override
        public ObjectMetadata clone() {
            return new TestObject(name, alias, objectType);
        }
    }

    static class TestColumn extends AbstractColumnMetadata {
        TestColumn(String name) {
            setName(name);
        }

        @Override
        public ObjectType getObjectType() {
            return Type.object;
        }
    }

    static class PrefixCodec implements ValueCodec<String, String> {
        @Override
        public String encodeNull() {
            return "encoded:null";
        }

        @Override
        public String encode(Object value) {
            return "encoded:" + value;
        }

        @Override
        public String decode(Object data) {
            return String.valueOf(data).replace("encoded:", "");
        }
    }

    static class SimpleDictionaryCodec implements DictionaryCodec {
        @Override
        public Collection<Object> getItems() {
            return Collections.emptyList();
        }

        @Override
        public String getFieldName() {
            return "dict";
        }

        @Override
        public Object encode(Object value) {
            return "dict:" + value;
        }

        @Override
        public Object decode(Object data) {
            return "dict:decoded:" + data;
        }
    }
}
