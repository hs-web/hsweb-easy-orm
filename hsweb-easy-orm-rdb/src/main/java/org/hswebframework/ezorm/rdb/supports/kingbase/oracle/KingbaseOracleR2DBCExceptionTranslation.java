package org.hswebframework.ezorm.rdb.supports.kingbase.oracle;

import io.r2dbc.spi.R2dbcException;
import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.exception.DuplicateKeyException;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.operator.ExceptionTranslation;

import java.util.Collections;

@AllArgsConstructor(staticName = "of")
public class KingbaseOracleR2DBCExceptionTranslation implements ExceptionTranslation {

    @SuppressWarnings("unused")
    private final RDBSchemaMetadata schema;

    @Override
    public Throwable translate(Throwable error) {
        if (error instanceof R2dbcException) {
            R2dbcException exception = (R2dbcException) error;
            if (KingbaseOracleJDBCExceptionTranslation.isDuplicateKey(
                exception.getErrorCode(),
                exception.getSqlState(),
                exception.getMessage()
            )) {
                return new DuplicateKeyException(false, Collections.emptyList(), error);
            }
        }
        return error;
    }
}
