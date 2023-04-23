package org.hswebframework.ezorm.rdb.codec;

import org.hswebframework.ezorm.core.Decoder;

import java.nio.ByteBuffer;
import java.sql.Blob;
import java.sql.Clob;

public class JdbcResultDecoder implements Decoder<Object> {

    public static final JdbcResultDecoder INSTANCE = new JdbcResultDecoder();

    @Override
    public Object decode(Object data) {
        if (data instanceof Blob || data instanceof ByteBuffer) {
            return ClobValueCodec.INSTANCE.decode(data);
        }

        if (data instanceof Clob) {
            return BlobValueCodec.INSTANCE.decode(data);
        }


        return data;
    }
}
