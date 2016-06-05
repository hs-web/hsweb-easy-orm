package og.hsweb.ezorm.meta.converter;

import og.hsweb.ezorm.meta.expand.ValueConverter;

/**
 * Created by zhouhao on 16-6-4.
 */
public class DefaultValueConverter implements ValueConverter {
    @Override
    public Object getData(Object value) {
        return value;
    }

    @Override
    public Object getValue(Object data) {
        return data;
    }
}
