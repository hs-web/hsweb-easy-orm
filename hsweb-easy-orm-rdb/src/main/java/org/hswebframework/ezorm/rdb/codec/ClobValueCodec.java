package org.hswebframework.ezorm.rdb.codec;


import lombok.SneakyThrows;
import org.hswebframework.ezorm.core.ValueCodec;

import javax.sql.rowset.serial.SerialClob;
import java.io.Reader;
import java.sql.Clob;

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

        return new SerialClob(value.toString().toCharArray());
    }

    @Override
    @SneakyThrows
    public Object decode(Object data) {
        if (data instanceof Clob) {
            Clob clobData = ((Clob) data);
            data = clobData.getSubString(1, (int) clobData.length());
        }
        return data;
    }
}
