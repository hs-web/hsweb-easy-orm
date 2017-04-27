package org.hsweb.ezorm.rdb.meta.expand;


import org.hsweb.commons.StringUtils;
import org.hsweb.ezorm.core.ObjectWrapper;
import org.hsweb.ezorm.rdb.meta.converter.BlobValueConverter;
import org.hsweb.ezorm.rdb.meta.converter.ClobValueConverter;

import java.sql.Blob;
import java.sql.Clob;
import java.util.LinkedHashMap;
import java.util.Map;

public class SimpleMapWrapper implements ObjectWrapper<Map<String, Object>> {

    private static final BlobValueConverter blobValueConverter = new BlobValueConverter();

    private static final ClobValueConverter clobValueConverter = new ClobValueConverter();

    @Override
    public Class<LinkedHashMap> getType() {
        return LinkedHashMap.class;
    }

    @Override
    public Map<String, Object> newInstance() {
        return new LinkedHashMap<>();
    }

    @Override
    public void wrapper(Map<String, Object> instance, int index, String attr, Object value) {
        if ("ROWNUM_".equals(attr.toUpperCase())) return;
        putValue(instance, attr, value);
    }

    @Override
    public boolean done(Map<String, Object> instance) {
        return true;
    }

    protected Object convertValue(Object value) {
        if (value instanceof Blob)
            return blobValueConverter.getValue(value);
        if (value instanceof Clob)
            return clobValueConverter.getValue(value);
        return value;
    }

    private void putValue(Map<String, Object> instance, String attr, Object value) {
        value = convertValue(value);
        if (attr.contains(".")) {
            String[] attrs = StringUtils.splitFirst(attr, "[.]");
            String attr_ob_name = attrs[0];
            String attr_ob_attr = attrs[1];
            Object object = instance.computeIfAbsent(attr_ob_name, k -> newInstance());
            if (object instanceof Map) {
                Map<String, Object> objectMap = (Map) object;
                putValue(objectMap, attr_ob_attr, value);
            }
        } else {
            instance.put(attr, value);
        }
    }
}
