package org.hswebframework.ezorm.rdb.supports.mysql;

import io.r2dbc.spi.R2dbcException;
import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.exception.DuplicateKeyException;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.operator.ExceptionTranslation;

import java.util.Collections;

@AllArgsConstructor(staticName = "of")
public class MysqlR2DBCExceptionTranslation implements ExceptionTranslation {

    private final RDBSchemaMetadata schema;

    @Override
    public Throwable translate(Throwable e) {
        if (e instanceof R2dbcException) {
            R2dbcException exception = ((R2dbcException) e);
            if (exception.getErrorCode() == 1062 || exception.getErrorCode() == 1022) {
                throw new DuplicateKeyException(true, Collections.emptyList(), e);
            }
        }
        return e;
    }
}
