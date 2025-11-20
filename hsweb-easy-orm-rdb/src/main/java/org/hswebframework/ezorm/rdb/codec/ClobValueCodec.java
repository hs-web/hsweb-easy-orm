package org.hswebframework.ezorm.rdb.codec;


import io.netty.buffer.ByteBuf;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.core.meta.ColumnMetadata;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.supports.oracle.OracleDialect;
import org.hswebframework.ezorm.rdb.utils.FeatureUtils;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.sql.Clob;
import java.sql.JDBCType;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

public class ClobValueCodec implements ValueCodec {

    public static final ClobValueCodec INSTANCE = new ClobValueCodec();

    public static boolean isClobType(DataType type) {
        return type.getSqlType() == JDBCType.LONGVARCHAR ||
            type.getSqlType() == JDBCType.LONGNVARCHAR ||
            type.getSqlType() == JDBCType.CLOB;
    }

    @Override
    public Object encodeNull(ColumnMetadata column) {
        if (column instanceof RDBColumnMetadata col) {
            if (ClobValueCodec.isClobType(col.getType())) {
                return NullValue.of(LongCharSequence.class, col.getType());
            }
            return NullValue.of(col.getType());
        }
        return null;
    }

    @Override
    public Object encode(Object value, ColumnMetadata column) {
        Object val = this.encode(value);
        if (val instanceof CharSequence cs && column instanceof RDBColumnMetadata col) {
            if (ClobValueCodec.isClobType(col.getType())) {
                return new LongCharSequence(cs);
            }
        }
        return val;
    }

    @Override
    @SneakyThrows
    public Object encode(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof Clob) {
            return value;
        }
        if (value instanceof String) {
            return value;
        }

        if (value instanceof ByteBuffer) {
            return value;
        }

        if (value instanceof ByteBuf) {
            return ((ByteBuf) value).nioBuffer();
        }


        return value.toString();
    }

    @Override
    @SneakyThrows
    public Object decode(Object data) {
        if (data instanceof ByteBuffer) {
            ByteBuffer byteBuffer = ((ByteBuffer) data);
            return StandardCharsets.UTF_8.decode(byteBuffer).toString();
        }
        if (data instanceof Clob) {
            Clob clobData = ((Clob) data);
            return clobData.getSubString(1, (int) clobData.length());
        }
        if (FeatureUtils.r2dbcIsAlive()) {
            Mono<?> mono = null;
            if (data instanceof io.r2dbc.spi.Clob) {
                mono = Flux.from(((io.r2dbc.spi.Clob) data).stream())
                           .collect(Collectors.joining());
            }
            if (mono != null) {
                // TODO: 2019-09-25 更好的方式？
                return mono.toFuture().get(10, TimeUnit.SECONDS);
            }
        }
        return data;
    }
}
