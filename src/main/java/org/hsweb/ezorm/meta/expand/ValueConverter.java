package org.hsweb.ezorm.meta.expand;

public interface ValueConverter {
    Object getData(Object value);

    Object getValue(Object data);
}
