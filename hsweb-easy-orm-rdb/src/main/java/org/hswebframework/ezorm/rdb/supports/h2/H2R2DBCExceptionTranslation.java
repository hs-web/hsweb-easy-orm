package org.hswebframework.ezorm.rdb.supports.h2;

import io.r2dbc.spi.R2dbcException;
import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.exception.DuplicateKeyException;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.operator.ExceptionTranslation;

import java.util.Collections;

@AllArgsConstructor(staticName = "of")
public class H2R2DBCExceptionTranslation implements ExceptionTranslation {

    private final RDBSchemaMetadata schema;

    @Override
    public Throwable translate(Throwable e) {
        if (e instanceof R2dbcException) {
            R2dbcException exception = ((R2dbcException) e);
            if (exception.getErrorCode() == 23505) {
                throw new DuplicateKeyException(true, Collections.emptyList(), e);
            }
        }
        return e;
    }
}
