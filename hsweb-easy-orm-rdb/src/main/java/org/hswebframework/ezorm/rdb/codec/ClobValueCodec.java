package org.hswebframework.ezorm.rdb.codec;


import lombok.SneakyThrows;
import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.rdb.utils.FeatureUtils;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.sql.Clob;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

public class ClobValueCodec implements ValueCodec {

    public static final ClobValueCodec INSTANCE = new ClobValueCodec();

    @Override
    @SneakyThrows
    public Object encode(Object value) {

        if (value instanceof Clob) {
            return (value);
        }
        if (value instanceof String) {
            return value;
        }

        return value.toString();
    }

    @Override
    @SneakyThrows
    public Object decode(Object data) {
        if (data instanceof Clob) {
            Clob clobData = ((Clob) data);
            data = clobData.getSubString(1, (int) clobData.length());
        } else if (FeatureUtils.r2dbcIsAlive()) {
            Mono mono = null;
            if (data instanceof io.r2dbc.spi.Clob) {
                mono = Flux.from(((io.r2dbc.spi.Clob) data).stream())
                        .collect(Collectors.joining());
            }
            if (mono != null) {
                // TODO: 2019-09-25 更好的方式？
                return mono.toFuture().get(10, TimeUnit.SECONDS);
            }
        }else if(data instanceof ByteBuffer){
            ByteBuffer byteBuffer = ((ByteBuffer) data);

            byte[] bytes = new byte[byteBuffer.remaining()];

            byteBuffer.get(bytes);
            return new String(bytes, StandardCharsets.UTF_8);
        }

        return data;
    }
}
