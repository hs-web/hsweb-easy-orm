package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.operator.ExceptionTranslation;

public class PostgresqlJdbcExceptionTranslation implements ExceptionTranslation {


    @Override
    public Throwable translate(Throwable e) {


        return e;
    }
}
