package org.hswebframework.ezorm.rdb.codec;

import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.LengthSupport;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.junit.Assert;
import org.junit.Test;

import java.util.List;
import java.util.Map;

public class JsonValueCodecBranchTest {

    @Test
    public void testEncodeBranches() {
        JsonValueCodec codec = JsonValueCodec.of(Map.class);
        Assert.assertNull(codec.encode(null));
        Assert.assertEquals("raw", codec.encode("raw"));
        Assert.assertEquals("{\"name\":\"JetLinks\"}", codec.encode(Map.of("name", "JetLinks")));
        Assert.assertEquals("{\"name\":\"JetLinks\"}", codec.encode(Map.of("name", "JetLinks"), column(new SimpleDataType("json"))));
    }

    @Test
    public void testEncodeNullAndClobBranches() {
        JsonValueCodec codec = JsonValueCodec.of(Map.class);
        RDBColumnMetadata clobColumn = column(new ClobDataType());
        RDBColumnMetadata jsonColumn = column(new SimpleDataType("json"));

        Object clob = codec.encodeNull(clobColumn);
        Assert.assertTrue(clob instanceof NullValue);
        Assert.assertEquals(LongCharSequence.class, ((NullValue) clob).getType());

        Object json = codec.encodeNull(jsonColumn);
        Assert.assertTrue(json instanceof NullValue);
        Assert.assertEquals(jsonColumn.getType(), ((NullValue) json).getDataType());
        Assert.assertNull(codec.encodeNull());
    }

    @Test
    public void testDecodeBranches() {
        JsonValueCodec codec = JsonValueCodec.of(Map.class);
        Assert.assertNull(codec.decode(null));
        Assert.assertNull(codec.decode("plain"));
        Assert.assertTrue(codec.decode("{\"a\":1}") instanceof Map);
        Assert.assertNull(codec.decode(new byte[]{'[', '1', ',', '2', ']'}));
        Assert.assertTrue(codec.decode(java.nio.ByteBuffer.wrap("{\"a\":1}".getBytes())) instanceof Map);
    }

    @Test
    public void testOfFieldMonoFluxAndArrayBranches() throws Exception {
        Assert.assertTrue(JsonValueCodec.ofField(Holder.class.getDeclaredField("list")).decode("[\"a\",\"b\"]") instanceof List);
        Assert.assertTrue(JsonValueCodec.ofField(Holder.class.getDeclaredField("map")).decode("{\"a\":1}") instanceof Map);
        Assert.assertNotNull(JsonValueCodec.ofField(Holder.class.getDeclaredField("mono")));
        Assert.assertNotNull(JsonValueCodec.ofField(Holder.class.getDeclaredField("flux")));
        Assert.assertTrue(JsonValueCodec.ofField(Holder.class.getDeclaredField("array")).decode("[\"a\",\"b\"]") instanceof String[]);
    }

    @Test
    public void testColumnLengthAndTypeBranches() {
        RDBTableMetadata table = new H2SchemaMetadata("PUBLIC").newTable("test_codec_branch");
        RDBColumnMetadata column = table.newColumn();
        column.setName("data");
        column.setDataType("varchar(64)");
        column.setType(new SimpleLengthDataType("varchar", 64, 10, 2));

        Assert.assertEquals("varchar(64)", column.getDataType());
        Assert.assertEquals(64, column.getLength());
        Assert.assertEquals(10, column.getPrecision());
        Assert.assertEquals(2, column.getScale());

        column.setLength(32);
        Assert.assertEquals(32, column.getLength());

        RDBTableMetadata afterTable = new H2SchemaMetadata("PUBLIC").newTable("test_codec_branch_2");
        RDBColumnMetadata after = afterTable.newColumn();
        after.setName("data");

        after.setType(new SimpleLengthDataType("varchar", 128, 12, 3));
        table.addColumn(column);
        afterTable.addColumn(after);
        Assert.assertTrue(column.ddlModifiable(after));
        Assert.assertFalse(after.ddlModifiable(after));
    }

    private static RDBColumnMetadata column(DataType type) {
        RDBTableMetadata table = new H2SchemaMetadata("PUBLIC").newTable("test_codec_branch");
        RDBColumnMetadata column = table.newColumn();
        column.setName("data");
        column.setType(type);
        return column;
    }

    public static class Holder {
        private List<String> list;
        private Map<String, Integer> map;
        private reactor.core.publisher.Mono<String> mono;
        private reactor.core.publisher.Flux<String> flux;
        private String[] array;
    }

    private record SimpleDataType(String id) implements DataType {
        @Override public String getId() { return id; }
        @Override public String getName() { return id; }
        @Override public java.sql.SQLType getSqlType() { return java.sql.JDBCType.VARCHAR; }
        @Override public Class<?> getJavaType() { return Object.class; }
    }

    private record ClobDataType() implements DataType, LengthSupport {
        @Override public String getId() { return "clob"; }
        @Override public String getName() { return "clob"; }
        @Override public java.sql.SQLType getSqlType() { return java.sql.JDBCType.CLOB; }
        @Override public Class<?> getJavaType() { return Object.class; }
        @Override public int getLength() { return 0; }
        @Override public int getScale() { return 0; }
        @Override public int getPrecision() { return 0; }
    }

    private record SimpleLengthDataType(String id, int length, int precision, int scale) implements DataType, LengthSupport {
        @Override public String getId() { return id; }
        @Override public String getName() { return id; }
        @Override public java.sql.SQLType getSqlType() { return java.sql.JDBCType.VARCHAR; }
        @Override public Class<?> getJavaType() { return Object.class; }
        @Override public int getLength() { return length; }
        @Override public int getScale() { return scale; }
        @Override public int getPrecision() { return precision; }
    }


    @Test
    public void testDecodeInputStreamReaderClobBlobAndFallbackBranches() throws Exception {
        JsonValueCodec codec = JsonValueCodec.of(Map.class);
        String json = "{\"a\":1}";
        Assert.assertTrue(codec.decode(new java.io.ByteArrayInputStream(json.getBytes())) instanceof Map);
        Assert.assertTrue(codec.decode(new java.io.StringReader(json)) instanceof Map);
        Assert.assertTrue(codec.decode(new javax.sql.rowset.serial.SerialClob(json.toCharArray())) instanceof Map);
        Assert.assertTrue(codec.decode(new javax.sql.rowset.serial.SerialBlob(json.getBytes())) instanceof Map);
        Assert.assertTrue(codec.decode(io.r2dbc.spi.Blob.from(reactor.core.publisher.Mono.just(java.nio.ByteBuffer.wrap(json.getBytes())))) instanceof Map);
        Assert.assertTrue(codec.decode(io.r2dbc.spi.Clob.from(reactor.core.publisher.Flux.just(json))) instanceof Map);
        Object fallback = codec.decode(new Object());
        Assert.assertSame(fallback, fallback);
    }
}
