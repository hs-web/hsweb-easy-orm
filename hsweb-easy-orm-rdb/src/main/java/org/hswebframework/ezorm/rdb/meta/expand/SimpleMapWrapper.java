package org.hswebframework.ezorm.rdb.meta.expand;


import org.hswebframework.ezorm.core.ObjectWrapper;
import org.hswebframework.ezorm.rdb.meta.converter.BlobValueCodec;
import org.hswebframework.ezorm.rdb.meta.converter.ClobValueCodec;

import java.sql.Blob;
import java.sql.Clob;
import java.util.LinkedHashMap;
import java.util.Map;

public class SimpleMapWrapper implements ObjectWrapper<Map<String, Object>> {

    private static final BlobValueCodec blobValueConverter = new BlobValueCodec();

    private static final ClobValueCodec clobValueConverter = new ClobValueCodec();

    public static final SimpleMapWrapper INSTANCE=new SimpleMapWrapper();

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
        if ("ROWNUM_".equals(attr.toUpperCase())) {
            return;
        }
        putValue(instance, attr, value);
    }

    @Override
    public boolean done(Map<String, Object> instance) {
        return true;
    }

    protected Object convertValue(Object value) {
        if (value instanceof Blob)
            return blobValueConverter.decode(value);
        if (value instanceof Clob)
            return clobValueConverter.decode(value);
        return value;
    }

    @SuppressWarnings("all")
    public void putValue(Map<String, Object> instance, String attr, Object value) {
        value = convertValue(value);
        if (attr.contains(".")) {
            String[] attrs = attr.split("[.]", 2);

            Object nest = instance.computeIfAbsent(attrs[0], k -> newInstance());
            Map<String, Object> tmp;
            if (nest instanceof Map) {
                tmp = (Map) nest;
            } else {
                instance.put(attrs[0], tmp = newInstance());
                instance.put(attrs[0].concat("_old"), value);
            }
            for (attrs = attrs[1].split("[.]", 2);
                 attrs.length > 1;
                 attrs = attrs[1].split("[.]", 2)) {
                String field = attrs[0];
                Object _nest = tmp.computeIfAbsent(field, k -> newInstance());
                if (_nest instanceof Map) {
                    tmp = (Map) _nest;
                } else {
                    tmp.put(field, tmp = newInstance());
                    tmp.put("_this_old", _nest);
                }
            }
            tmp.put(attrs[0], value);

        } else {
            instance.put(attr, value);
        }
    }
}
