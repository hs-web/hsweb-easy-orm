package org.hsweb.ezorm.meta.converter;

import org.hsweb.ezorm.meta.expand.ValueConverter;

import java.io.Reader;
import java.sql.Clob;

/**
 * Created by zhouhao on 16-6-5.
 */
public class ClobValueConverter implements ValueConverter {
    @Override
    public Object getData(Object value) {
        return value;
    }

    @Override
    public Object getValue(Object data) {
        if (data instanceof Clob) {
            try {
                Reader reader = ((Clob) data).getCharacterStream();
                char[] chars = new char[(int) ((Clob) data).length()];
                reader.read(chars);
                data = new String(chars);
            } catch (Exception e) {
            }
        }
        return data;
    }
}
