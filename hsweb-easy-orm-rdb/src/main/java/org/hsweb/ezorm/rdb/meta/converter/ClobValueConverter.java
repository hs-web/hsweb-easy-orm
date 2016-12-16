package org.hsweb.ezorm.rdb.meta.converter;


import org.hsweb.ezorm.core.ValueConverter;

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
