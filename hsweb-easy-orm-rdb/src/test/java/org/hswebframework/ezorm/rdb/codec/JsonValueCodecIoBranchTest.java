package org.hswebframework.ezorm.rdb.codec;

import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.LengthSupport;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.junit.Assert;
import org.junit.Test;

import javax.sql.rowset.serial.SerialBlob;
import javax.sql.rowset.serial.SerialClob;
import java.io.ByteArrayInputStream;
import java.io.StringReader;
import java.nio.ByteBuffer;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;

public class JsonValueCodecIoBranchTest {

    @Test
    public void testDecodeClobBlobInputStreamReaderAndByteBuffer() throws Exception {
        JsonValueCodec codec = JsonValueCodec.of(Map.class);
        Assert.assertTrue(codec.decode(new SerialClob("{\"name\":1}".toCharArray())) instanceof Map);
        Assert.assertTrue(codec.decode(new SerialBlob("{\"name\":2}".getBytes())) instanceof Map);
        Assert.assertTrue(codec.decode(new ByteArrayInputStream("{\"name\":3}".getBytes())) instanceof Map);
        Assert.assertTrue(codec.decode(new StringReader("{\"name\":4}")) instanceof Map);
        Assert.assertTrue(codec.decode(ByteBuffer.wrap("{\"name\":5}".getBytes())) instanceof Map);
        Assert.assertTrue(codec.decode("{\"name\":6}") instanceof Map);
    }

    @Test
    public void testEncodeNullAndEncodeWithClobColumnBranches() {
        JsonValueCodec codec = JsonValueCodec.of(Map.class);
        RDBColumnMetadata clob = column(new ClobLikeType());
        Object encodedNull = codec.encodeNull(clob);
        Assert.assertTrue(encodedNull instanceof NullValue);
        Assert.assertEquals(LongCharSequence.class, ((NullValue) encodedNull).getType());

        Object encoded = codec.encode(Map.of("name", "JetLinks"), clob);
        Assert.assertTrue(encoded instanceof LongCharSequence);
        Assert.assertTrue(encoded.toString().contains("JetLinks"));

        Assert.assertEquals("raw", String.valueOf(codec.encode("raw", clob)));
    }

    @Test
    public void testTargetTypePreservesInstanceAndCollectionFactory() {
        JsonValueCodec codec = JsonValueCodec.ofCollection(List.class, String.class);
        Assert.assertTrue(codec.decode("[\"a\",\"b\"]") instanceof List);
        Assert.assertEquals("[\"a\",\"b\"]", codec.encode(List.of("a", "b")));
    }

    private static RDBColumnMetadata column(DataType type) {
        RDBTableMetadata table = new H2SchemaMetadata("PUBLIC").newTable("json_codec_branch");
        RDBColumnMetadata column = table.newColumn();
        column.setName("data");
        column.setType(type);
        return column;
    }

    private record ClobLikeType() implements DataType, LengthSupport {
        @Override
        public String getId() {
            return "clob";
        }

        @Override
        public String getName() {
            return "clob";
        }

        @Override
        public java.sql.SQLType getSqlType() {
            return java.sql.JDBCType.CLOB;
        }

        @Override
        public Class<?> getJavaType() {
            return Object.class;
        }

        @Override
        public int getLength() {
            return 0;
        }

        @Override
        public int getScale() {
            return 0;
        }

        @Override
        public int getPrecision() {
            return 0;
        }
    }
}
