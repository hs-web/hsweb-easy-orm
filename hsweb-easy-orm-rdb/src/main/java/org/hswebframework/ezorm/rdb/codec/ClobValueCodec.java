package org.hswebframework.ezorm.rdb.codec;


import lombok.SneakyThrows;
import org.hswebframework.ezorm.core.ValueCodec;

import java.io.Reader;
import java.sql.Clob;

public class ClobValueCodec implements ValueCodec {

    public static final ClobValueCodec INSTANCE = new ClobValueCodec();

    @Override
    public Object encode(Object value) {

        return value;
    }

    @Override
    @SneakyThrows
    public Object decode(Object data) {
        if (data instanceof Clob) {
            Clob clobData = ((Clob) data);
            data = clobData.getSubString(0, (int) clobData.length());
        }
        return data;
    }
}
