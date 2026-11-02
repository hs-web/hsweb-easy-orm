package org.hswebframework.ezorm.rdb.executor;

import io.r2dbc.spi.Statement;

public interface R2dbcParameterBinder {

    void bind(Statement statement, String identifier);
}
