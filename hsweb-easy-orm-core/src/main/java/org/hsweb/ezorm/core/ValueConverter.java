package org.hsweb.ezorm.core;

public interface ValueConverter {
    Object getData(Object value);

    Object getValue(Object data);
}
