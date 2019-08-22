package org.hswebframework.ezorm.rdb.codec;


import org.hswebframework.ezorm.core.ValueCodec;

import java.io.Reader;
import java.sql.Clob;

public class ClobValueCodec implements ValueCodec {
    @Override
    public Object encode(Object value) {
        return value;
    }

    @Override
    public Object decode(Object data) {
        if (data instanceof Clob) {
            Clob clobData = ((Clob) data);
            try (Reader reader = clobData.getCharacterStream()) {
                char[] chars = new char[(int) clobData.length()];
                reader.read(chars);
                data = new String(chars);
            } catch (Exception e) {
            }
        }
        return data;
    }
}
