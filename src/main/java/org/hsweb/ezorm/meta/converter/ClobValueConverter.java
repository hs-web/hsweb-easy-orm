package org.hsweb.ezorm.meta.converter;

import org.hsweb.ezorm.meta.expand.ValueConverter;

import java.io.Reader;
import java.sql.Clob;

public class ClobValueConverter implements ValueConverter {
    @Override
    public Object getData(Object value) {
        return value;
    }

    @Override
    public Object getValue(Object data) {
        if (data instanceof Clob) {
            try {
                Clob clobData = ((Clob) data);
                Reader reader = clobData.getCharacterStream();
                char[] chars = new char[(int) clobData.length()];
                reader.read(chars);
                data = new String(chars);
            } catch (Exception e) {
            }
        }
        return data;
    }
}
