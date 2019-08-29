package org.hswebframework.ezorm.core;

import java.util.Optional;

public interface ObjectPropertyOperator {

    Optional<Object> getProperty(Object object, String name);

    void setProperty(Object object, String name, Object value);

}
